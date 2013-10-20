module dpick.ast;

//
class DataExpr
{
    DataPiece[] items;
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

}