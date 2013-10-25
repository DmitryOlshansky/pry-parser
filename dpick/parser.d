module dpick.parser;

import dpick.ast;

import std.algorithm, std.range, std.exception, std.ascii, std.typecons;

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
            skipWs(false); //may hit EOF
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
            Expr e = parseExpression(), e2;
            if(match(","))
                e2 = parseExpression();            
            check("}");
            return new DataPiece(a, e, e2 ? e2 : e);
        }
        return new DataPiece(a, new Number(low), new Number(high));
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
        bool ignore = match('!');
        string name = null;
        skipWs();
        if(isAlpha(input.front))
            name = parseId();
        AliasAtom[] aliases = null;
        if(match(':'))
        {
            for(;;)
            {
                skipWs();
                if(!isAlpha(input.front))
                    break;
                aliases ~= parseAliasAtom();
            }
        }
        return new AliasExpr(ignore, name, aliases);
    }

    //AliasAtom : Expr '->' Name
    AliasAtom parseAliasAtom()
    {
        Expr e = parseExpression();
        check("->");
        string name = parseId();
        return new AliasAtom(name, e);
    }

    //EntityExpr : Name 
    //           : BytePattern
    //           : StringPattern
    EntityExpr parseEntityExpr()
    {
        skipWs();
        if(match('\"'))
        {
            return parseStringPattern();
        }
        if(input.front == '[' || isDigit(input.front))
            return parseBytePattern();
        if(isAlpha(input.front))
            return new NameExpr(parseId());
        error(`expected one of '"', digit or alphabetic character`);
        assert(0);
    }

    //StringPattern : '"' CharClass+ '"'
    //1st quote already matched
    StringPattern parseStringPattern()
    {        
        return new StringPattern(null);
    }

    //BytePattern : ByteClass+
    BytePattern parseBytePattern()
    {
        ByteClass[] pat = null;
        for(;;)
        {   
            skipWs();
            if(match("["))
            {
                auto mask = new ByteMask;
                do
                {
                    auto pair = parseRangeExpr();
                    mask.mark(pair[0], pair[1]);
                }while(!match("]"));
                pat ~= mask;
                continue;
            }
            if(isDigit(input.front))
            {
                int v = parseNum();
                pat ~= new Byte(cast(ubyte)v);
                continue;
            }
            break;
        }
        return new BytePattern(null);
    }

    //TODO: full expression tree, use operator precedence grammar
    Expr parseExpression()
    {
        //just numbers for now
        return new Number(parseNum());
    }

    //RangeExpr : Number
    //          : Number '-' Number
    auto parseRangeExpr()
    {
        int first = parseNum();
        int second = first + 1;
        if(match('-'))
        {
            second = parseNum();
        }
        return tuple(first, second);
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

    void skipWs(bool failOnEof=true)
    {
        for(;;)
        {
            if(input.empty)
            {
                failOnEof && error("unexpected end of input");
                break;
            }
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

unittest
{
    enum tests = (){
        auto d = Parser("root = 0x0 ;").parse();
        assert("root" in d);
        return d;
    };
    enum ctTests = tests();
    tests();
}