module dpick.parser;

import dpick.ast;

import std.algorithm, std.range, std.exception, std.ascii;

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
    enum INF = -1;
    string input;
    int line;
    DataExpr[string] decls;

    this(string inp)
    {
        input = inp;
        line = 1;
    }

    auto parse()
    {
        skipWs();
        while(!input.empty)
        {            
            parseDeclaration();
            skipWs();
        }
        if("root" !in decls)
            error("no 'root' entity defined");
        return decls;
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
        //falthrough - 1 item is also a sequence
        return new DataSeq(pieces);
    }

    //DataPiece : DataAtom
    //          : DataAtom '*'
    //          : DataAtom '?'
    //          : DataAtom '{' Expr (',' Expr )? '}'
    DataPiece parseDataPiece()
    {
        DataAtom a = parseDataAtom();
        int low = 1, high = 1;
        if(match("?"))
        {
            low = 0;
        }
        else if(match("*")) 
        {
            low = 0;
            high = INF;
        }
        else if(match("+"))
        {
            low = 1;
            high = INF;
        }
        else if(match("{"))
        {

        }
        return new DataPiece(a, low, high);
    }

    //DataAtom : EntityExpr AliasExpr 
    //         : '(' DataExpr ')' AliasExpr
    DataAtom parseDataAtom()
    {
        if(match('('))
        {
            DataExpr expr = parseDataExpr();
            auto ret =  new ExprAtom(expr, parseAliasExpr());
            check(')');
            return ret;
        }        
        EntityExpr expr = parseEntityExpr();        
        return new EntityAtom(expr, parseAliasExpr());
    }

    //AliasExpr : '!'? Name? (':' AliasAtom*)?
    AliasExpr parseAliasExpr()
    {
        return new AliasExpr();
    }

    //EntityExpr : Name 
    //           : BytePattern
    //           : StringPattern
    EntityExpr parseEntityExpr()
    {
        return new EntityExpr();
    }

    //Name : [a-zA-Z_][a-zA-Z_0-9]*
    string parseId()
    {
        skipWs();
        auto save = input; 
        if(!isAlpha(input.front))
            error("expected alphabetic character");
        input.popFront();        
        input = input.find!(x => !isAlpha(x) && !isDigit(x));
        return save[0..$-input.length];
    }

    //Number : [0-9]+
    //       : '0' 'x' [0-9A-Fa-f]+
    int parseNum()
    {
        skipWs();
        if(!isDigit(input.front))
            error("expected digit character");        
        int val = input.front - '0';
        input.popFront();
        if(val == 0 && input.front == 'x')
        {
            input.popFront();
            return parseHex();
        }
        while(!input.empty)
        {
            auto ch = input.front;
            if(!isDigit(ch))
                break;
            val = 10 * val + ch - '0';
            if(val > 255)
                error("values may not exceed the range [0, 255]");
            input.popFront();
        }
        return val;
    }

    unittest
    {
        assert(collectException(Parser("0x").parseNum()));
        assert(Parser("0x81").parseNum() == 0x81);
        assert(Parser("12").parseNum() == 12);
        assert(Parser(" 01").parseNum() == 1);
    }

    // [0-9A-Fa-f]+
    int parseHex()
    {
        int val = 0;
        if(input.empty)
            error("unexpected end of input");
        if(!isHexDigit(input.front))
            error("expected hex-digit character");
        do
        {
            auto ch = input.front;
            int d;
            if(ch >= '0' && ch <= '9')
                d = ch - '0';
            else if(ch >= 'A' && ch <= 'F') 
                d = ch - 'A' + 10;
            else if(ch >= 'a' && ch <= 'f') 
                d = ch - 'a' + 10;
            else
                break;
            val = 16 * val + d;
            if(val > 255)
                error("values may not exceed the range [0, 255]");
            input.popFront();
        }while(!input.empty);
        return val;        
    }

    unittest
    {
        assert(Parser("12").parseHex() ==  0x12);
        assert(Parser("FF").parseHex() ==  255);
        assert(Parser("aB").parseHex() ==  0xaB);
        assert(collectException(Parser("Ga").parseHex()));
        assert(Parser("Ag").parseHex() == 10);
    }

    bool match(const(char)[] piece...)
    {
        skipWs();
        if(input.skipOver(piece))
        {
            return true;
        }
        return false;
    }

    void check(const(char)[] piece...)
    {
        if(!match(piece))
            error("expected: " ~ piece.idup);
    }

    void skipWs()
    {
        for(;;)
        {
            auto f = input.front;
            if(!isWhite(f))
                break;
            if(f == '\n')
                line++;
            input.popFront();
        }
    }

    unittest
    {
        assert(Parser(" \nabc").match("abc"));
        assert(Parser("abc").match('a'));
    }

    void error(string msg)
    {
        throw new ParseException(line, msg);
    }
}