module dpick.ast;

abstract class Ast
{
@safe pure:
    abstract void accept(Visitor walker);    
}

mixin template Visitable()
{
    override void accept(Visitor w)
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
@safe pure:
    this(DataPiece[] pieces)
    {
        super(pieces);
    }
    mixin Visitable;
}

class DataSeq : DataExpr
{
@safe pure:    
    this(DataPiece[] pieces)
    {
        super(pieces);
    }
    mixin Visitable;
}

class DataPiece : Ast
{
@safe pure:
    DataAtom atom;
    Expr low, high;
    this(DataAtom a, Expr l, Expr h)
    {
        atom = a;
        low = l;
        high = h;
    }
    mixin Visitable;
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
@safe pure:
    EntityExpr entity;
    this(EntityExpr e, AliasExpr a)
    {
        super(a);
        entity = e;
    }
    mixin Visitable;
}

class ExprAtom : DataAtom
{
@safe pure:
    DataExpr expr;
    this(DataExpr e, AliasExpr a)
    {
        super(a);
        expr = e;        
    }
    mixin Visitable;
}

class AliasExpr : Ast
{
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
    mixin Visitable;
}

class AliasAtom : Ast
{
@safe pure:
    string id;
    Expr expr;    
    this(string name, Expr e)
    {
        id = name;
        expr = e;
    }
    mixin Visitable;
}

class EntityExpr : Ast {}

class NameExpr : EntityExpr
{
@safe pure:
    string id;
    this(string name)
    {
        id = name;
    }
    mixin Visitable;
}

class BytePattern : EntityExpr
{
@safe pure:
    ByteClass[] pattern;
    this(ByteClass[] pat)
    {
        pattern = pat;
    }
    mixin Visitable;
}

class StringPattern : EntityExpr
{
@safe pure:
    CharClass[] pattern;
    this(CharClass[] pat)
    {
        pattern = pat;
    }
    mixin Visitable;   
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
@safe pure:
    BitMask!8 mask;
    alias mask this;
    mixin Visitable;
}

class Byte : ByteClass
{
@safe pure:    
    ubyte value;
    this(ubyte val)
    {
        value  = val;
    }
    mixin Visitable;
}

class CharClass : Ast{}

class CharMask : CharClass
{
@safe pure:
    //TODO: + set of char for non-ascii part of UTF-8
    BitMask!7 ascii;
    alias ascii this;
    mixin Visitable;
}

class Char : CharClass
{
@safe pure:     
    char ch;
    this(char c)
    {
        ch = c;
    }
    mixin Visitable;
}

class Expr : Ast
{

}

class Number : Expr
{
@safe pure:
    int value;
    this(int v)
    {
        value = v;
    }
    mixin Visitable;    
}

class Variable: Expr
{
@safe pure:
    string id;
    this(string id)
    {
        this.id = id;
    }
    mixin Visitable;
}

string nullVistorFor(T...)()
{
    static if(T.length != 0)
    {
        return `void visit(`~T[0].stringof~` arg){ }`
        ~ nullVistorFor!(T[1..$]);
    }
    else
        return "";
}

class Visitor
{
@safe pure:
    mixin(nullVistorFor!(
        Expr, StringPattern, BytePattern, NameExpr,
        AliasAtom, AliasExpr, EntityAtom, ExprAtom, 
        DataPiece, DataSeq, DataAlt,
        ByteMask, Byte, CharMask, Char
    ));
}