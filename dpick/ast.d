module dpick.ast;

abstract class Ast
{
@safe pure:
    abstract bool accept(Walker walker);    
}

mixin template Visitable()
{
    override bool accept(Walker w)
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
    int low, high;
    this(DataAtom a, int l, int h)
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

struct ByteClass
{

}

struct CharClass
{

}

class Expr : Ast
{
@safe pure:    
    mixin Visitable;
}

string simpleVistorFor(T...)()
{
    static if(T.length != 0)
    {
        return `bool visit(`~T[0].stringof~` arg){ return true; }`
        ~ simpleVistorFor!(T[1..$]);
    }
    else
        return "";
}

class Walker
{
@safe pure:
    mixin(simpleVistorFor!(
        Expr, StringPattern, BytePattern, NameExpr,
        AliasAtom, AliasExpr, EntityAtom, ExprAtom, 
        DataPiece, DataSeq, DataAlt
    ));
}