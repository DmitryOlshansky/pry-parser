module dpick.ast;

import std.typetuple, std.traits;

alias Seq = TypeTuple;

abstract class Ast
{
    abstract bool accept(Visitor walker);    
    override string toString()
    {
        import std.array;
        auto app = appender!string();
        this.writeTo((const(char)[] s)=> app.put(s));
        return app.data;
    }
}

mixin template Visitable()
{
    override bool accept(Visitor w)
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
        return `bool visit(`~T[0].stringof~` arg){ return stopFlag; }`
        ~ nullVistorFor!(T[1..$]);
    }
    else
        return "";
}

alias AstLeafTypes =  TypeTuple!(
    BinExpr, UnExpr, Number, Variable,
    StringPattern, BytePattern, NameExpr, 
    AliasAtom, AliasExpr, EntityAtom, ExprAtom, 
    DataPiece, DataSeq, DataAlt,
    ByteMask, Byte, CharMask, Char
);

template EraseIf(alias pred, T...)
{
    static if(T.length > 0)
    {
        static if(pred!(T[0]))
            alias EraseIf = EraseIf!(pred, T[1..$]);
        else
            alias EraseIf = Seq!(T[0], EraseIf!(pred, T[1..$]));
    }
    else
        alias EraseIf = Seq!();
}

template SubtypesOf(Base, T...)
{
    enum isNotSubtype(U) = !is(U : Base);
    alias SubtypesOf = EraseIf!(isNotSubtype, T);
}

//pragma(msg, SubtypesOf!(Expr, AstLeafTypes));

class Visitor
{
    bool stopFlag;
    public void stop(){ stopFlag = true; }
    mixin(nullVistorFor!(AstLeafTypes));
}

enum isUnary(alias Fn) = arity!Fn == 1;

private string generateAdhocVisitor(Types...)(bool withRet)
{
    import std.conv;
    string ret;
    foreach(i, t; Types)
    {
        ret ~= `override bool visit(`~t.stringof~` arg){`
            ~(withRet ? `ret = ` : ``)~`Fns[`
            ~to!string(i)~`](arg); return stopFlag; }`;
    }
    return ret;
}

public auto matcher(Fns...)()
    if(allSatisfy!(isCallable, Fns) && allSatisfy!(isUnary, Fns))
{
    import std.typecons;
    alias Args = staticMap!(ParameterTypeTuple, Fns);
    alias Rets = staticMap!(ReturnType, Fns);
    //if types are different, do not return anything    
    enum hasReturn = !is(CommonType!Rets == void);
    
    class Matcher : Visitor {        
        static if(hasReturn) {
            CommonType!Rets ret;
            @property auto value(){ return ret; }
        }
        //pragma(msg, generateAdhocVisitor!(Args)(hasReturn));
        mixin(generateAdhocVisitor!(Args)(hasReturn));
    };
    //@@@BUG@@@ static var of  nested class segfaults at R-T
    //@@@BUG@@@ ditto with scoped!Matcher - segfaults at R-T
    return new Matcher();
}

enum TraverseMode {
    inOrder,
    postOrder
}

bool _depthFirst(TraverseMode mode=inOrder)(Ast node, Visitor visitor)
{
    static if(mode == inOrder)
        if(!e.accept(visitor))
            return false;
    bool m = ast.match!(
        (BinExpr e)=> e.left._depthFirst(visitor) ?
                e.right._depthFirst(visitor) : false,
        (UnExpr e) => e.arg._depthFirst(visitor), 
        (Number n) => true,
        (Variable var) => true,
        (StringPattern pat) => true,
        (BytePattern pat) => true,
        (NameExpr  n) => true,
        (AliasAtom a) => a.expr._depthFirst(visitor), 
        (AliasExpr e){
            foreach(a; e.others){
                if(!a._depthFirst(visitor))
                    return false;
            }
            return true;
        },
        (EntityAtom a) => a.entity._depthFirst(visitor),
        (ExprAtom a) => a.expr._depthFirst(visitor), 
        (DataPiece dp) {
            if(dp.atom._depthFirst(visitor))
            {
                return dp.low._depthFirst(visitor) ? 
                    dp.high._depthFirst(visitor) : false;
            }
            return false;
        },
        (DataSeq seq)  {
            foreach(a; seq.items){
                if(!a._depthFirst(visitor))
                    return false;
            }
            return true;
        },
        (DataAlt alt)  {
            foreach(a; alt.items){
                if(!a._depthFirst(visitor))
                    return false;
            }
            return true;
        },
        (ByteMask mask) =>true,
        (Byte b)  => true,
        (CharMask mask) => true,
        (Char ch)  => true,
    );
    if(!m)
        return false;
    static if(mode == postOrder)
        return e.accept(visitor);
}


public auto match(Fns...)(Ast node)
    if(allSatisfy!(isCallable, Fns) && allSatisfy!(isUnary, Fns))
{
    auto m = matcher!(Fns);
    node.accept(m);
    static if(is(typeof(m.value)))
        return m.value;
}

public auto depthFirst(Fns...)(Ast node)
    if(allSatisfy!(isCallable, Fns) && allSatisfy!(isUnary, Fns))
{
    auto m = matcher!(Fns);
    node.depthFirst(m);
    static if(is(typeof(m.value)))
        return m.value;
}

void writeTo(Ast ast, scope void delegate (const(char)[]) sink)
{
    import std.format;
    ast.match!(
        (BinExpr e){
            e.left.writeTo(sink);
            sink(e.op);
            e.right.writeTo(sink);
        },
        (UnExpr e){
            sink(e.op);
            e.arg.writeTo(sink);
        }, 
        (Number n) => formattedWrite(sink, "%d", n.value),
        (Variable var) => sink(var.id),
        (StringPattern pat) => sink("StringPat"),
        (BytePattern pat) => sink("BytePat"),
        (NameExpr  n){
            sink("Name(");
            sink(n.id);
            sink(")");
        },
        (AliasAtom a){
            sink(a.id);
            sink(" -> ");
            a.expr.writeTo(sink);
        }, 
        (AliasExpr e){
            foreach(a; e.others){
                a.writeTo(sink);
            }
        },
        (EntityAtom a) => sink("_"),
        (ExprAtom a) => sink("_"), 
        (DataPiece dp) => sink("_"),
        (DataSeq seq) => sink("_"),
        (DataAlt alt) => sink("_"),
        (ByteMask mask) => sink("_"),
        (Byte b) => formattedWrite(sink, " 0x%2x ", b.value),
        (CharMask mask) => sink("_"),
        (Char ch) => sink((&ch.ch)[0..1]),
    );
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