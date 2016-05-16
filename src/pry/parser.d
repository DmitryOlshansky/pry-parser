module pry.parser;

import pry.ast, pry.misc;

import std.algorithm, std.range, std.exception, std.typecons;
import std.typetuple;

alias Seq = TypeTuple;

static import ascii = std.ascii;

pure @safe nothrow bool isAlpha()(dchar c)
{
    return ascii.isAlpha(c) || c == '_';
}

alias isDigit = ascii.isDigit;
alias isWhite = ascii.isWhite;
alias isHexDigit = ascii.isHexDigit;

enum anonymous = "__anonymous__";

class ParseException : Exception
{
@safe pure:
    this(int line, string suffix, string msg)
    {
        import std.conv;
        auto context = suffix.length > 16 ? suffix[0..16] ~ "..." : suffix;
        super("Line #"~to!string(line)~" before `"~context~"`: "~msg);
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

    //Declaration : id ':' DataExpr ';'
    void parseDeclaration()
    {
        string id = parseId();
        check(':');
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
        DataSeq[] pieces;
        pieces ~= parseDataSeq();
        //alternation
        //DataAlternative : DataSeq ('|' DataSeq )+
        if(match('|'))
        {
            do
            {
                pieces ~= parseDataSeq();
            }
            while(match('|'));
            
        }
        return new DataAlt(pieces);
    }
    
    //sequence
    //DataSequence : DataPiece+ (',' DataPiece+)*
    DataSeq parseDataSeq()
    {
        DataPiece[] pieces;
        auto save = input.save;
        string name = anonymous;
        try {
            name = parseId();
            if(!match("="))
            {
                name = anonymous;
                input = save;
            }
        }
        catch(Exception)
        {
            input = save;
        }

        pieces ~= parseDataPiece;
        if(match(','))
        {
            do
            {
                pieces ~= parseDataPiece();
            }
            while(match(','));
        }
        return new DataSeq(name, pieces);
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
            check(')');
            return new ExprAtom(expr);
        }
        skipWs();
        if(match('\"'))
        {
            return parseStringPattern();
        }
        if(input.front == '[' || isDigit(input.front))
            return parseBytePattern();
        if(isAlpha(input.front))
            return new NameAtom(parseId());
        error(`expected one of '"', digit or alphabetic character`);
        assert(0);
    }


    //StringPattern : '"' CharClass+ '"'
    //1st quote already matched
    DataAtom parseStringPattern()
    {
        StringAtom[] pat;
        //CharClass : [^\\\[\]]
        //          : '\' [\\\[\]]
        //          : '[' '^'? RangeExpr+ ']'
    OuterLoop:
        for(;;)
        {
            //TODO: must handle full Unicode with std.uni stuff
            //once it is CTFE-able
            auto ch = input.front;
            switch(ch)
            {
                case '[':
                    auto mask = new CharPattern;
                    do
                    {
                        auto pair = parseRangeExpr();
                        mask.mark(pair[0], pair[1]);
                    }while(!match("]"));
                    pat ~= mask;
                    break;
                case '\\':
                    input.popFront();
                    if(input.empty)
                        error("unterminated escape sequence");
                    ch = input.front;
                    if(ch != '\\' && ch != '[' && ch != ']' && ch !='"')
                        error("incorrect escape sequence");
                    break;
                case '"':
                    input.popFront();
                    break OuterLoop;
                default:
                    //TODO: dchar --> many Char's
                    pat ~= new Char(cast(char)ch);
                    break;
            }
            input.popFront();
            if(input.empty)
                error("unterminated string pattern");
        }
        return new StringPattern(pat);
    }

    // BytePattern
    DataAtom parseBytePattern()
    {
        DataAtom pat = null;
        skipWs();
        if(match("["))
        {
            auto mask = new BytePattern;
            do
            {
                auto pair = parseRangeExpr();
                mask.mark(pair[0], pair[1]);
            }while(!match("]"));
            pat = mask;
        }
        else if(isDigit(input.front))
        {
            int v = parseNum();
            pat = new Byte(cast(ubyte)v);
        }
        return pat;
    }

    struct Op {
        string tok; //slice of str that matches
        //-1 - end paren, 0 - start paren
        //else higher - greater priority
        int  priority;
        int  arity;
    }
    static auto binOp(string tok, int priority)
    {
        return Op(tok, priority, 2);
    }
    enum TERMINATOR = 1, OPEN = 0, CLOSE = -1;
    alias operators = Seq!(
        binOp("*", 30), binOp("/", 30), binOp("%", 30),
        binOp("+", 20), binOp("-", 20),
        binOp("<<", 10), binOp(">>", 10),
        binOp("<", 8), binOp(">", 8),
        binOp("==", 7),
        binOp("&", 5),
        binOp("^", 4),
        binOp("|", 3),
        binOp(".", 2),
        //delimeters these terminate the expresion
        Op("}", TERMINATOR, 0), Op(",", TERMINATOR, 0),
        Op("(", OPEN, 0xBEAF), Op(")", CLOSE, 0xBEAF)
    );
    static immutable opTable = [ operators ];

    Op matchOp()
    {
        auto ch = input.front;
        switch(ch)
        {
            foreach(startIdx, op; operators)
            {
                //if haven't seen another operator starting on this token
                //TODO: can simplify this by preparing the list beforehand
                static if (!opTable[0..startIdx].canFind!"a.tok[0] == b.tok[0]"(op))
                {
        case  op.tok[0]:
                    //maximal munch - try all longer operators first
                    input.popFront();
                    if(input.empty)
                        error("unterminated expression");
                    foreach (idx, op2; operators)
                    {
                        static if(op2.tok.length > 1 && op2.tok[0] == op.tok[0])
                        {
                            if(input.skipOver(op2.tok[1..$]))
                                return operators[idx];
                        }
                    }
                    foreach(idx, op2; operators)
                    {
                        static if(op2.tok.length == 1 && op2.tok[0] == op.tok[0])
                        {
                            return operators[idx];
                        }
                    }
                }
            }
        default:
            error("unrecognized operator");
        }
        assert(0);
    }



    //TODO: full expression tree, use operator precedence grammar
    Expr parseExpression()
    {
        Stack!Op opStack;
        Stack!Expr valStack;
        void collapseStackUntil(bool delegate(Op ) pure @safe pred)
        {
            while(!opStack.empty && pred(opStack.top))
            {
                Op op = opStack.pop();
                if(op.arity == 1)
                    valStack.top = new UnExpr(valStack.top, op.tok);
                else if(op.arity == 2)
                {
                    Expr e = valStack.pop();
                    valStack.top = new BinExpr(valStack.top, op.tok, e);
                }
            }
        }
        void pushOp(Op op)
        {
            if(op.priority == CLOSE)
            {
                collapseStackUntil(arg => arg.priority == OPEN);
                if(opStack.empty)
                    error("unmatched '(' in expression");
                assert(opStack.top.priority == OPEN);
                opStack.pop();
            }
            else if(op.priority == OPEN)
                opStack.push(op);
            else
            {
                collapseStackUntil(arg => arg.priority >= op.priority);
            }
            opStack.push(op);
        }
        void pushVar(Expr val)
        {
            valStack.push(val);
        }
        for(;;)
        {
            skipWs();
            auto ch = input.front;
            if(isDigit(ch))
                pushVar(new Number(parseNum()));
            else if(isAlpha(ch))
                pushVar(new Variable(parseId()));
            else
            {
                auto save = input;
                Op op = matchOp();
                if(op.priority == TERMINATOR)
                {
                    //it's not our job to check if it's the right kind
                    input = save; //hence rollback
                    break;
                }
                pushOp(op);
            }
        }
        //TODO: squash the stack and yeild final result
        collapseStackUntil(op => true);
        return valStack.top;
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

            //line comment
            if(input.skipOver("//"))
            {
                input = find(input, '\n');
                if(!input.empty)
                    input.popFront();
                line++;
                continue;
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
        throw new ParseException(line, input, msg);
    }
}

unittest
{
    import std.stdio, std.conv;
    enum tests = (){
        auto d = Parser("root = 0x0 ;").parse();
        assert("root" in d);
        d = Parser(import("json.dpick")).parse();
        //writeln(d["root"]);
        d = Parser(import("bson.dpick")).parse();
        writeln(d["binary"]);
        return d;
    };
    //enum ctTests = tests();
    tests();
    Expr e = Parser("id*2+3, ").parseExpression();
    assert(e.match!((BinExpr be){
        assert(be.left.match!((BinExpr e) => true), "left isn't binary");
        assert(be.right.match!((Number n) => n.value == 3), "borked right");
        return be.op == "+";
    }), "not binary expr");
    //use first-match with default
    assert(e.match!((UnExpr e) => false, (Ast a)=>true, (BinExpr b)=>false));
    assert(e.to!string == "id*2+3");
}