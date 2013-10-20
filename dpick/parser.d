module dpick.parser;

import dpick.ast;

import std.algorithm, std.range, std.exception, std.uni;

class ParseException : Exception
{
@safe pure:
    this(int line, string msg)
    {
        import std.conv;
        super(to!string(line)~":"~msg);
    }
}

private struct Parser
{
@safe pure:
    string input;
    int line;
    DataExpr[string] decls;

    this(string inp)
    {
        input = inp;
        line = 1;
        skipWs();
        while(!input.empty)
        {            
            parseDeclaration();
            skipWs();
        }
        if("root" !in decls)
            error("no 'root' entity defined");
    }

    //Declaration : id '=' DataExpr ';'
    void parseDeclaration()
    {
        string id = parseId();
        check('=');
        DataExpr expr = parseDataExpr();
        check(';');
        if(id in decls)
            error("double definition of "~id);
        decls[id] = expr;
    }

    DataPiece parseDataPiece()
    {
        return new DataPiece();
    }

    //DataExpr : DataSequence
    //         : DataAlternative
    DataExpr parseDataExpr()
    {
        DataPiece[] pieces;
        pieces ~= parseDataPiece();
        //alternation
        //DataAlternative : DataPiece ('|' DataPiece )+
        if(match('|'))
        {
            do
            {
                pieces ~= parseDataPiece();
            }
            while(match('|'));            
            return new DataAlt(pieces);
        }
        //sequence
        //DataSequence : DataPiece (',' DataPiece)*
        if(match(','))
        {
            do
            {
                pieces ~= parseDataPiece();
            }
            while(match(','));
        }
        //falthrough - 1 item is also sequence
        return new DataSeq(pieces);
    }

    //Name : [a-zA-Z_][a-zA-Z_0-9]*
    string parseId()
    {
        skipWs();
        auto save = input; 
        if(!isAlpha(input.front))
            error("expected alphabetic character");
        input.popFront();        
        input = input.find!(x => !isAlpha(x) && !isNumber(x));
        return save[0..$-input.length];
    }

    bool match(char[] piece...)
    {
        skipWs();
        if(input.skipOver(piece))
        {
            return true;
        }
        return false;
    }

    void check(char[] piece...)
    {
        if(!match(piece))
            error("expected: " ~ piece.idup);
    }

    void skipWs()
    {
        for(;;)
        {
            auto f = input.front;
            if(f == '\n')
                line++;
            input.popFront();
        }
    }

    void error(string msg)
    {
        throw new ParseException(line, msg);
    }
}