module dpick.ast;

import std.typetuple, std.traits;

abstract class Ast
{
    abstract void accept(const Visitor walker);    
}

mixin template Visitable()
{
    override void accept(const Visitor w)
    {
        return w.visit(this);
    }
}

//
class DataExpr : Ast
{
@safe pure:
    DataPiece[] items;
    this(DataPiece[] pieces)
    {
        items = pieces;
    }
}

class DataAlt : DataExpr
{
    mixin Visitable;
@safe pure:
    this(DataPiece[] pieces)
    {
        super(pieces);
    }    
}

class DataSeq : DataExpr
{
    mixin Visitable;
@safe pure:    
    this(DataPiece[] pieces)
    {
        super(pieces);
    }    
}

class DataPiece : Ast
{
    mixin Visitable;
@safe pure:
    DataAtom atom;
    Expr low, high;
    this(DataAtom a, Expr l, Expr h)
    {
        atom = a;
        low = l;
        high = h;
    }
}

class DataAtom : Ast
{
@safe pure:
    AliasExpr aliasExpr;
    this(AliasExpr a)
    {
        aliasExpr = a;
    }
}

class EntityAtom : DataAtom
{
    mixin Visitable;
@safe pure:
    EntityExpr entity;
    this(EntityExpr e, AliasExpr a)
    {
        super(a);
        entity = e;
    }
}

class ExprAtom : DataAtom
{
    mixin Visitable;
@safe pure:
    DataExpr expr;
    this(DataExpr e, AliasExpr a)
    {
        super(a);
        expr = e;        
    }
}

class AliasExpr : Ast
{
    mixin Visitable;
@safe pure:
    bool ignorable;
    string primary;
    AliasAtom[] others;
    this(bool ignore, string primeId, AliasAtom[] a)
    {
        ignorable = ignore;
        primary = primeId;
        others = a;
    }    
}

class AliasAtom : Ast
{
    mixin Visitable;
@safe pure:
    string id;
    Expr expr;    
    this(string name, Expr e)
    {
        id = name;
        expr = e;
    }    
}

class EntityExpr : Ast {}

class NameExpr : EntityExpr
{
    mixin Visitable;
@safe pure:
    string id;
    this(string name)
    {
        id = name;
    }
}

class BytePattern : EntityExpr
{
    mixin Visitable;
@safe pure:
    ByteClass[] pattern;
    this(ByteClass[] pat)
    {
        pattern = pat;
    }
}

class StringPattern : EntityExpr
{
    mixin Visitable;
@safe pure:
    CharClass[] pattern;
    this(CharClass[] pat)
    {
        pattern = pat;
    }
}

struct BitMask(size_t bitSize)
{
@safe pure:
    uint[(1<<bitSize)/32] mask;

    void mark(int start, int end)
    {
        //TODO: optimize, e.g. can mark a word at at time
        for(int idx = start; idx<end; idx++)
        {
            mask[idx/32] |= 1<<(idx%32);
        }
    }

    uint opIndex(uint idx)
    {
        return mask[idx/32] & 1<<idx;
    }

    void invert()
    {
        foreach(ref w; mask)
            w = ~w;
    }
}

class ByteClass : Ast{}

class ByteMask : ByteClass
{
    mixin Visitable;
@safe pure:
    BitMask!8 mask;
    alias mask this;
}

class Byte : ByteClass
{
    mixin Visitable;
@safe pure:    
    ubyte value;
    this(ubyte val)
    {
        value  = val;
    }
}

class CharClass : Ast{}

class CharMask : CharClass
{
    mixin Visitable;
@safe pure:
    //TODO: + set of char for non-ascii part of UTF-8
    BitMask!7 ascii;
    alias ascii this;
}

class Char : CharClass
{
    mixin Visitable;
@safe pure:     
    char ch;
    this(char c)
    {
        ch = c;
    }
}

class Expr : Ast
{

}

class UnExpr : Expr
{
    mixin Visitable;
@safe pure:
    string op;
    Expr arg;
    this(Expr e, string opTok)
    {
        op = opTok;
        arg = e;
    }
}

class BinExpr : Expr
{
    mixin Visitable;
@safe pure:
    string op;
    Expr left, right;
    this(Expr e, string opTok, Expr e2)
    {
        left = e;
        op = opTok;
        right = e2;
    }
}

class Number : Expr
{
    mixin Visitable;
@safe pure:
    int value;
    this(int v)
    {
        value = v;
    }
}

class Variable: Expr
{
    mixin Visitable;
@safe pure:
    string id;
    this(string id)
    {
        this.id = id;
    }
}

string nullVistorFor(T...)()
{
    static if(T.length != 0)
    {
        return `void visit(`~T[0].stringof~` arg)const{ }`
        ~ nullVistorFor!(T[1..$]);
    }
    else
        return "";
}

class Visitor
{
    mixin(nullVistorFor!(
        Expr, StringPattern, BytePattern, NameExpr,
        AliasAtom, AliasExpr, EntityAtom, ExprAtom, 
        DataPiece, DataSeq, DataAlt,
        ByteMask, Byte, CharMask, Char
    ));
}

enum isUnary(alias Fn) = arity!Fn == 1;

private string generateAdhocVisitor(Types...)(bool withRet)
{
    import std.conv;
    string ret;
    foreach(i, t; Types)
    {
        ret ~= `override void visit(`~t.stringof~` arg)const{`
            ~(withRet ? `ret = ` : ``)~`Fns[`
            ~to!string(i)~`](arg); }`;
    }
    return ret;
}

auto match(Fns...)(Ast node)
    if(allSatisfy!(isCallable, Fns) && allSatisfy!(isUnary, Fns))
{
    import std.typecons;
    alias Args = staticMap!(ParameterTypeTuple, Fns);
    alias Rets = staticMap!(ReturnType, Fns);
    static assert(NoDuplicates!Rets.length == 1, "return types must be the same");
    enum hasReturn = !is(Rets[0] == void);
    static if(hasReturn)
        Rets[0] ret;
    class Matcher : Visitor {
        mixin(generateAdhocVisitor!(Args)(hasReturn));
    };
    //@@@BUG@@@ should be able to be static but segfaults at R-T
    //@@@BUG@@@ should be able to be scoped!Matcher but segfaults at R-T
    Matcher m = new Matcher();
    node.accept(m);
    static if(hasReturn)
        return ret;
}

unittest
{
    import std.stdio;
    auto n = new NameExpr("Name");
    auto e = new Byte(90);

    alias M = match!(
        (NameExpr n) @trusted => 0,
        (Byte b) @trusted => 1
    );
    assert(M(n) == 0);
    assert(M(e) == 1);
}