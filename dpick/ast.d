module dpick.ast;

//
class DataExpr
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
}

class DataSeq : DataExpr
{
@safe pure:    
    this(DataPiece[] pieces)
    {
        super(pieces);
    }
}

class DataPiece
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
}

class DataAtom
{
@safe pure:
    AliasExpr aliases;
    this(AliasExpr a)
    {
        aliases = a;
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
}

class AliasExpr
{

}

class EntityExpr
{

}