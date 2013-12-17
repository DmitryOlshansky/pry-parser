//Stripped down proof of conecept for std.regex on top of Buffer ranges
module regex;

import std.internal.uni, std.internal.uni_tab;//Unicode property tables
import std.array, std.algorithm, std.range,
       std.conv, std.exception, std.traits, std.typetuple,
       std.utf, std.format, std.typecons, std.bitmanip,
       std.functional, std.exception;

import core.bitop, core.stdc.string, core.stdc.stdlib;
static import ascii = std.ascii;
import std.string : representation;

debug(std_regex_parser) import std.stdio; //trace parser progress
debug(std_regex_search) import std.stdio; //trace prefix search engine
debug(std_regex_matcher) import std.stdio;  //trace matcher engine
debug(std_regex_allocation) import std.stdio; //track object life cycle
debug(std_regex_ctr) import std.stdio; //dump ctRegex generated sources
debug(std_regex_test) import std.stdio; //trace test suite progress

private:

import std.uni : isAlpha, isWhite;


// IR bit pattern: 0b1_xxxxx_yy
// where yy indicates class of instruction, xxxxx for actual operation code
//     00: atom, a normal instruction
//     01: open, opening of a group, has length of contained IR in the low bits
//     10: close, closing of a group, has length of contained IR in the low bits
//     11 unused
//
// Loops with Q (non-greedy, with ? mark) must have the same size / other properties as non Q version
// Possible changes:
//* merge group, option, infinite/repeat start (to never copy during parsing of (a|b){1,2})
//* reorganize groups to make n args easier to find, or simplify the check for groups of similar ops
//  (like lookaround), or make it easier to identify hotspots.

enum IR:uint {
    Char              = 0b1_00000_00, //a character
    Any               = 0b1_00001_00, //any character
    CodepointSet      = 0b1_00010_00, //a most generic CodepointSet [...]
    Trie              = 0b1_00011_00, //CodepointSet implemented as Trie
    //match with any of a consecutive OrChar's in this sequence
    //(used for case insensitive match)
    //OrChar holds in upper two bits of data total number of OrChars in this _sequence_
    //the drawback of this representation is that it is difficult
    // to detect a jump in the middle of it
    OrChar            = 0b1_00100_00,
    Nop               = 0b1_00101_00, //no operation (padding)
    End               = 0b1_00110_00, //end of program
    Bol               = 0b1_00111_00, //beginning of a string ^
    Eol               = 0b1_01000_00, //end of a string $
    Wordboundary      = 0b1_01001_00, //boundary of a word
    Notwordboundary   = 0b1_01010_00, //not a word boundary
    Backref           = 0b1_01011_00, //backreference to a group (that has to be pinned, i.e. locally unique) (group index)
    GroupStart        = 0b1_01100_00, //start of a group (x) (groupIndex+groupPinning(1bit))
    GroupEnd          = 0b1_01101_00, //end of a group (x) (groupIndex+groupPinning(1bit))
    Option            = 0b1_01110_00, //start of an option within an alternation x | y (length)
    GotoEndOr         = 0b1_01111_00, //end of an option (length of the rest)
    //... any additional atoms here

    OrStart           = 0b1_00000_01, //start of alternation group  (length)
    OrEnd             = 0b1_00000_10, //end of the or group (length,mergeIndex)
    //with this instruction order
    //bit mask 0b1_00001_00 could be used to test/set greediness
    InfiniteStart     = 0b1_00001_01, //start of an infinite repetition x* (length)
    InfiniteEnd       = 0b1_00001_10, //end of infinite repetition x* (length,mergeIndex)
    InfiniteQStart    = 0b1_00010_01, //start of a non eager infinite repetition x*? (length)
    InfiniteQEnd      = 0b1_00010_10, //end of non eager infinite repetition x*? (length,mergeIndex)
    RepeatStart       = 0b1_00011_01, //start of a {n,m} repetition (length)
    RepeatEnd         = 0b1_00011_10, //end of x{n,m} repetition (length,step,minRep,maxRep)
    RepeatQStart      = 0b1_00100_01, //start of a non eager x{n,m}? repetition (length)
    RepeatQEnd        = 0b1_00100_10, //end of non eager x{n,m}? repetition (length,step,minRep,maxRep)
    //
    LookaheadStart    = 0b1_00101_01, //begin of the lookahead group (length)
    LookaheadEnd      = 0b1_00101_10, //end of a lookahead group (length)
    NeglookaheadStart = 0b1_00110_01, //start of a negative lookahead (length)
    NeglookaheadEnd   = 0b1_00110_10, //end of a negative lookahead (length)
    LookbehindStart   = 0b1_00111_01, //start of a lookbehind (length)
    LookbehindEnd     = 0b1_00111_10, //end of a lookbehind (length)
    NeglookbehindStart= 0b1_01000_01, //start of a negative lookbehind (length)
    NeglookbehindEnd  = 0b1_01000_10, //end of negative lookbehind (length)
}

//a shorthand for IR length - full length of specific opcode evaluated at compile time
template IRL(IR code)
{
    enum uint IRL =  lengthOfIR(code);
}

static assert (IRL!(IR.LookaheadStart) == 3);

//how many parameters follow the IR, should be optimized fixing some IR bits
int immediateParamsIR(IR i){
    switch (i){
    case IR.OrEnd,IR.InfiniteEnd,IR.InfiniteQEnd:
        return 1;
    case IR.RepeatEnd, IR.RepeatQEnd:
        return 4;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        return 2;
    default:
        return 0;
    }
}

//full length of IR instruction inlcuding all parameters that might follow it
int lengthOfIR(IR i)
{
    return 1 + immediateParamsIR(i);
}

//full length of the paired IR instruction inlcuding all parameters that might follow it
int lengthOfPairedIR(IR i)
{
    return 1 + immediateParamsIR(pairedIR(i));
}

//if the operation has a merge point (this relies on the order of the ops)
bool hasMerge(IR i)
{
    return (i&0b11)==0b10 && i <= IR.RepeatQEnd;
}

//is an IR that opens a "group"
bool isStartIR(IR i)
{
    return (i&0b11)==0b01;
}

//is an IR that ends a "group"
bool isEndIR(IR i)
{
    return (i&0b11)==0b10;
}

//is a standalone IR
bool isAtomIR(IR i)
{
    return (i&0b11)==0b00;
}

//makes respective pair out of IR i, swapping start/end bits of instruction
IR pairedIR(IR i)
{
    assert(isStartIR(i) || isEndIR(i));
    return cast(IR)(i ^ 0b11);
}

//encoded IR instruction
struct Bytecode
{
    uint raw;
    //natural constraints
    enum maxSequence = 2+4;
    enum maxData = 1<<22;
    enum maxRaw = 1<<31;

    this(IR code, uint data)
    {
        assert(data < (1<<22) && code < 256);
        raw = code<<24 | data;
    }

    this(IR code, uint data, uint seq)
    {
        assert(data < (1<<22) && code < 256 );
        assert(seq >= 2 && seq < maxSequence);
        raw = code << 24 | (seq - 2)<<22 | data;
    }

    //store raw data
    static Bytecode fromRaw(uint data)
    {
        Bytecode t;
        t.raw = data;
        return t;
    }

    //bit twiddling helpers
    @property uint data() const { return raw & 0x003f_ffff; }

    //ditto
    @property uint sequence() const { return 2 + (raw >> 22 & 0x3); }

    //ditto
    @property IR code() const { return cast(IR)(raw>>24); }

    //ditto
    @property bool hotspot() const { return hasMerge(code); }

    //test the class of this instruction
    @property bool isAtom() const { return isAtomIR(code); }

    //ditto
    @property bool isStart() const { return isStartIR(code); }

    //ditto
    @property bool isEnd() const { return isEndIR(code); }

    //number of arguments for this instruction
    @property int args() const { return immediateParamsIR(code); }

    //mark this GroupStart or GroupEnd as referenced in backreference
    void setBackrefence()
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        raw = raw | 1 << 23;
    }

    //is referenced
    @property bool backreference() const
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        return cast(bool)(raw & 1 << 23);
    }

    //mark as local reference (for backrefs in lookarounds)
    void setLocalRef()
    {
        assert(code == IR.Backref);
        raw = raw | 1 << 23;
    }

    //is a local ref
    @property bool localRef() const
    {
        assert(code == IR.Backref);
        return cast(bool)(raw & 1 << 23);
    }

    //human readable name of instruction
    @trusted @property string mnemonic() const
    {//@@@BUG@@@ to is @system
        return to!string(code);
    }

    //full length of instruction
    @property uint length() const
    {
        return lengthOfIR(code);
    }

    //full length of respective start/end of this instruction
    @property uint pairedLength() const
    {
        return lengthOfPairedIR(code);
    }

    //returns bytecode of paired instruction (assuming this one is start or end)
    @property Bytecode paired() const
    {//depends on bit and struct layout order
        assert(isStart || isEnd);
        return Bytecode.fromRaw(raw ^ 0b11 << 24);
    }

    //gets an index into IR block of the respective pair
    uint indexOfPair(uint pc) const
    {
        assert(isStart || isEnd);
        return isStart ? pc + data + length  : pc - data - lengthOfPairedIR(code);
    }
}

static assert(Bytecode.sizeof == 4);

//debugging tool, prints out instruction along with opcodes
@trusted string disassemble(in Bytecode[] irb, uint pc, in NamedGroup[] dict=[])
{
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch(irb[pc].code)
    {
    case IR.Char:
        formattedWrite(output, " %s (0x%x)",cast(dchar)irb[pc].data, irb[pc].data);
        break;
    case IR.OrChar:
        formattedWrite(output, " %s (0x%x) seq=%d", cast(dchar)irb[pc].data, irb[pc].data, irb[pc].sequence);
        break;
    case IR.RepeatStart, IR.InfiniteStart, IR.Option, IR.GotoEndOr, IR.OrStart:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+IRL!(IR.RepeatStart));
        break;
    case IR.RepeatEnd, IR.RepeatQEnd: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u",
            pc - len, irb[pc + 3].raw, irb[pc + 4].raw, irb[pc + 2].raw);
        break;
    case IR.InfiniteEnd, IR.InfiniteQEnd, IR.OrEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
        break;
    case  IR.LookaheadEnd, IR.NeglookaheadEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
        break;
    case IR.GroupStart, IR.GroupEnd:
        uint n = irb[pc].data;
        string name;
        foreach(v;dict)
            if(v.group == n)
            {
                name = "'"~v.name~"'";
                break;
            }
        formattedWrite(output, " %s #%u " ~ (irb[pc].backreference ? "referenced" : ""),
                name, n);
        break;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        uint len = irb[pc].data;
        uint start = irb[pc+1].raw, end = irb[pc+2].raw;
        formattedWrite(output, " pc=>%u [%u..%u]", pc + len + IRL!(IR.LookaheadStart), start, end);
        break;
    case IR.Backref: case IR.CodepointSet: case IR.Trie:
        uint n = irb[pc].data;
        formattedWrite(output, " %u",  n);
        if(irb[pc].code == IR.Backref)
            formattedWrite(output, " %s", irb[pc].localRef ? "local" : "global");
        break;
    default://all data-free instructions
    }
    if(irb[pc].hotspot)
        formattedWrite(output, " Hotspot %u", irb[pc+1].raw);
    return output.data;
}

//disassemble the whole chunk
@trusted void printBytecode()(in Bytecode[] slice, in NamedGroup[] dict=[])
{
    import std.stdio;
    for(uint pc=0; pc<slice.length; pc += slice[pc].length)
        writeln("\t", disassemble(slice, pc, dict));
}

//index entry structure for name --> number of submatch
struct NamedGroup
{
    string name;
    uint group;
}

//holds pair of start-end markers for a submatch
struct Group(DataIndex)
{
    DataIndex begin, end;
    @trusted string toString() const
    {
        auto a = appender!string();
        formattedWrite(a, "%s..%s", begin, end);
        return a.data;
    }
}

@trusted void reverseBytecode()(Bytecode[] code)
{
    Bytecode[] rev = new Bytecode[code.length];
    uint revPc = cast(uint)rev.length;
    Stack!(Tuple!(uint, uint, uint)) stack;
    uint start = 0;
    uint end = cast(uint)code.length;
    for(;;)
    {
        for(uint pc = start; pc < end; )
        {
            uint len = code[pc].length;
            if(code[pc].code == IR.GotoEndOr)
                break; //pick next alternation branch
            if(code[pc].isAtom)
            {
                rev[revPc - len .. revPc] = code[pc .. pc + len];
                revPc -= len;
                pc += len;
            }
            else if(code[pc].isStart || code[pc].isEnd)
            {
                //skip over other embedded lookbehinds they are reversed 
                if(code[pc].code == IR.LookbehindStart
                    || code[pc].code == IR.NeglookbehindStart)
                {
                    uint blockLen = len + code[pc].data
                         + code[pc].pairedLength;
                    rev[revPc - blockLen .. revPc] = code[pc .. pc + blockLen];
                    pc += blockLen;
                    revPc -= blockLen;
                    continue;
                }
                uint second = code[pc].indexOfPair(pc);
                uint secLen = code[second].length;
                rev[revPc - secLen .. revPc] = code[second .. second + secLen];
                revPc -= secLen;
                if(code[pc].code == IR.OrStart)
                {
                    //we pass len bytes forward, but secLen in reverse
                    uint revStart = revPc - (second + len - secLen - pc);
                    uint r = revStart;
                    uint i = pc + IRL!(IR.OrStart);
                    while(code[i].code == IR.Option)
                    {
                        if(code[i - 1].code != IR.OrStart)
                        {
                            assert(code[i - 1].code == IR.GotoEndOr);
                            rev[r - 1] = code[i - 1];
                        }
                        rev[r] = code[i];
                        auto newStart = i + IRL!(IR.Option);
                        auto newEnd = newStart + code[i].data;
                        auto newRpc = r + code[i].data + IRL!(IR.Option);
                        if(code[newEnd].code != IR.OrEnd)
                        {
                            newRpc--;
                        }
                        stack.push(tuple(newStart, newEnd, newRpc));
                        r += code[i].data + IRL!(IR.Option);                        
                        i += code[i].data + IRL!(IR.Option);
                    }
                    pc = i;
                    revPc = revStart;
                    assert(code[pc].code == IR.OrEnd);
                }
                else
                    pc += len;
            }
        }
        if(stack.empty)
            break;
        start = stack.top[0];
        end = stack.top[1];
        revPc = stack.top[2];
        stack.pop();
    }
    code[] = rev[];
}


//Regular expression engine/parser options:
// global - search  all nonoverlapping matches in input
// casefold - case insensitive matching, do casefolding on match in unicode mode
// freeform - ignore whitespace in pattern, to match space use [ ] or \s
// multiline - switch  ^, $ detect start and end of linesinstead of just start and end of input
enum RegexOption: uint {
    global = 0x1,
    casefold = 0x2,
    freeform = 0x4,
    nonunicode = 0x8,
    multiline = 0x10,
    singleline = 0x20
}
//do not reorder this list
alias RegexOptionNames = TypeTuple!('g', 'i', 'x', 'U', 'm', 's');
static assert( RegexOption.max < 0x80);
enum RegexInfo : uint { oneShot = 0x80 }
alias Escapables = TypeTuple!('[', ']', '\\', '^', '$', '.', '|', '?', ',', '-',
    ';', ':', '#', '&', '%', '/', '<', '>', '`',  '*', '+', '(', ')', '{', '}',  '~');

private enum NEL = '\u0085', LS = '\u2028', PS = '\u2029';

//test if a given string starts with hex number of maxDigit that's a valid codepoint
//returns it's value and skips these maxDigit chars on success, throws on failure
dchar parseUniHex(Char)(ref Char[] str, size_t maxDigit)
{
    //std.conv.parse is both @system and bogus
    enforce(str.length >= maxDigit,"incomplete escape sequence");
    uint val;
    for(int k = 0; k < maxDigit; k++)
    {
        auto current = str[k];//accepts ascii only, so it's OK to index directly
        if('0' <= current && current <= '9')
            val = val * 16 + current - '0';
        else if('a' <= current && current <= 'f')
            val = val * 16 + current -'a' + 10;
        else if('A' <= current && current <= 'F')
            val = val * 16 + current - 'A' + 10;
        else
            throw new Exception("invalid escape sequence");
    }
    enforce(val <= 0x10FFFF, "invalid codepoint");
    str = str[maxDigit..$];
    return val;
}

@system unittest //BUG canFind is system
{
    string[] non_hex = [ "000j", "000z", "FffG", "0Z"];
    string[] hex = [ "01", "ff", "00af", "10FFFF" ];
    int[] value = [ 1, 0xFF, 0xAF, 0x10FFFF ];
    foreach(v; non_hex)
        assert(collectException(parseUniHex(v, v.length)).msg
          .canFind("invalid escape sequence"));
    foreach(i, v; hex)
        assert(parseUniHex(v, v.length) == value[i]);
    string over = "0011FFFF";
    assert(collectException(parseUniHex(over, over.length)).msg
      .canFind("invalid codepoint"));
}

//heuristic value determines maximum CodepointSet length suitable for linear search
enum maxCharsetUsed = 6;

enum maxCachedTries = 8;

alias Trie = CodepointTrie!8;

Trie[const(CodepointSet)] trieCache;

//accessor with caching
@trusted Trie getTrie(in CodepointSet set)
{// @@@BUG@@@ 6357 almost all properties of AA are not @safe
    if(__ctfe || maxCachedTries == 0)
        return Trie(set);
    else
    {
        auto p = set in trieCache;
        if(p)
            return *p;
        if(trieCache.length == maxCachedTries)
        {
            // flush entries in trieCache
            trieCache = null;
        }
        return (trieCache[set] = Trie(set));
    }
}

//property for \w character class
@property CodepointSet wordCharacter()
{
    return memoizeExpr!("CodepointSet.init.add(unicodeAlphabetic).add(unicodeMn).add(unicodeMc)
        .add(unicodeMe).add(unicodeNd).add(unicodePc)")();
}

@property Trie wordTrie()
{
    return memoizeExpr!("Trie(wordCharacter)")();
}

auto memoizeExpr(string expr)()
{
    if(__ctfe)
        return mixin(expr);
    alias T = typeof(mixin(expr));
    static T slot;
    static bool initialized;
    if(!initialized)
    {
        slot =  mixin(expr);
        initialized = true;
    }
    return slot;
}

/+
    fetch codepoint set corresponding to a name (InBlock or binary property)
+/
@trusted const(CodepointSet) getUnicodeSet(in char[] name, bool negated,  bool casefold)
{
    alias ucmp = comparePropertyName;
    CodepointSet s;

    //unicode property
    //helper: direct access with a sanity check
    if(ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
    {
        s.add(unicodeLu).add(unicodeLl).add(unicodeLt)
            .add(unicodeLo).add(unicodeLm);
    }
    else if(ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter") == 0)
    {
        s.add(unicodeLl).add(unicodeLu).add(unicodeLt);//Title case
    }
    else if(ucmp(name, "M") == 0 || ucmp(name, "Mark") == 0)
    {
        s.add(unicodeMn).add(unicodeMc).add(unicodeMe);
    }
    else if(ucmp(name, "P") == 0 || ucmp(name, "Punctuation") == 0)
    {
        s.add(unicodePc).add(unicodePd).add(unicodePs).add(unicodePe)
            .add(unicodePi).add(unicodePf).add(unicodePo);
    }
    else if(ucmp(name, "S") == 0 || ucmp(name, "Symbol") == 0)
    {
        s.add(unicodeSm).add(unicodeSc).add(unicodeSk).add(unicodeSo);
    }
    else if(ucmp(name, "Z") == 0 || ucmp(name, "Separator") == 0)
    {
        s.add(unicodeZs).add(unicodeZl).add(unicodeZp);
    }
    else if(ucmp(name, "C") == 0 || ucmp(name, "Other") == 0)
    {
        s.add(unicodeCo).add(unicodeLo).add(unicodeNo)
            .add(unicodeSo).add(unicodePo);
    }
    else if(ucmp(name, "any") == 0)
        s.add(Interval(0,0x10FFFF));
    else if(ucmp(name, "ascii") == 0)
        s.add(Interval(0,0x7f));
    else
    {
        auto range = assumeSorted!((x,y) => ucmp(x.name, y.name) < 0)(unicodeProperties);
        //creating empty Codepointset is a workaround
        auto eq = range.lowerBound(UnicodeProperty(cast(string)name,CodepointSet.init)).length;
        enforce(eq != range.length && ucmp(name,range[eq].name) == 0,
            "invalid property name");
        s = range[eq].set.dup;
    }

    if(casefold)
        s = caseEnclose(s);
    if(negated)
        s.negate();
    return cast(const CodepointSet)s;
}

//basic stack, just in case it gets used anywhere else then Parser
@trusted struct Stack(T)
{
    T[] data;
    @property bool empty(){ return data.empty; }

    @property size_t length(){ return data.length; }

    void push(T val){ data ~= val;  }
    
    T pop()
    {
        assert(!empty);
        auto val = data[$ - 1];
        data = data[0 .. $ - 1];
        if(!__ctfe)
            data.assumeSafeAppend();
        return val;
    }

    @property ref T top()
    {
        assert(!empty);
        return data[$ - 1]; 
    }
}

//safety limits
enum maxGroupNumber = 2^^19;
enum maxLookaroundDepth = 16;
// *Bytecode.sizeof, i.e. 1Mb of bytecode alone
enum maxCompiledLength = 2^^18;
//amounts to up to 4 Mb of auxilary table for matching
enum maxCumulativeRepetitionLength = 2^^20;

alias BasicElementOf(Range) = Unqual!(ElementEncodingType!Range);

struct Parser(R)
    if (isForwardRange!R && is(ElementType!R : dchar))
{
    enum infinite = ~0u;
    dchar _current;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    Bytecode[] ir;       //resulting bytecode
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    Stack!(uint) fixupStack;  //stack of opened start instructions
    NamedGroup[] dict;   //maps name -> user group number
    //current num of group, group nesting level and repetitions step
    Stack!(uint) groupStack;
    uint nesting = 0;
    uint lookaroundNest = 0;
    uint counterDepth = 0; //current depth of nested counted repetitions
    const(CodepointSet)[] charsets;  //
    const(Trie)[] tries; //
    uint[] backrefed; //bitarray for groups

    @trusted this(S)(R pattern, S flags)
        if(isSomeString!S)
    {
        pat = origin = pattern;
        //reserve slightly more then avg as sampled from unittests
        if(!__ctfe)
            ir.reserve((pat.length*5+2)/4);
        parseFlags(flags);
        _current = ' ';//a safe default for freeform parsing
        next();
        try
        {
            parseRegex();
        }
        catch(Exception e)
        {
            error(e.msg);//also adds pattern location
        }
        put(Bytecode(IR.End, 0));
    }

    //mark referenced groups for latter processing
    void markBackref(uint n)
    {
        if(n/32 >= backrefed.length)
            backrefed.length = n/32 + 1;
        backrefed[n / 32] |= 1 << (n & 31);
    }

    @property dchar current(){ return _current; }

    bool _next()
    {
        if(pat.empty)
        {
            empty =  true;
            return false;
        }
        _current = pat.front;
        pat.popFront();
        return true;
    }

    void skipSpace()
    {
        while(isWhite(current) && _next()){ }
    }

    bool next()
    {
        if(re_flags & RegexOption.freeform)
        {
            bool r = _next();
            skipSpace();
            return r;
        }
        else
            return _next();
    }

    void put(Bytecode code)
    {
        enforce(ir.length < maxCompiledLength,
            "maximum compiled pattern length is exceeded");
        ir ~= code;
    }

    void putRaw(uint number)
    {
        enforce(ir.length < maxCompiledLength,
            "maximum compiled pattern length is exceeded");
        ir ~= Bytecode.fromRaw(number);
    }

    //parsing number with basic overflow check
    uint parseDecimal()
    {
        uint r = 0;
        while(ascii.isDigit(current))
        {
            if(r >= (uint.max/10))
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(current-'0');
            if(!next())
                break;
        }
        return r;
    }

    //parse control code of form \cXXX, c assumed to be the current symbol
    dchar parseControlCode()
    {
        enforce(next(), "Unfinished escape sequence");
        enforce(('a' <= current && current <= 'z') || ('A' <= current && current <= 'Z'),
            "Only letters are allowed after \\c");
        return current & 0x1f;
    }

    //
    @trusted void parseFlags(S)(S flags)
    {//@@@BUG@@@ text is @system
        foreach(ch; flags)//flags are ASCII anyway
        {
        L_FlagSwitch:
            switch(ch)
            {

                foreach(i, op; __traits(allMembers, RegexOption))
                {
                    case RegexOptionNames[i]:
                            if(re_flags & mixin("RegexOption."~op))
                                throw new RegexException(text("redundant flag specified: ",ch));
                            re_flags |= mixin("RegexOption."~op);
                            break L_FlagSwitch;
                }
                default:
                    throw new RegexException(text("unknown regex flag '",ch,"'"));
            }
        }
    }

    //parse and store IR for regex pattern
    @trusted void parseRegex()
    {
        fixupStack.push(0);
        groupStack.push(1);//0 - whole match
        auto maxCounterDepth = counterDepth;
        uint fix;//fixup pointer

        while(!empty)
        {
            debug(std_regex_parser)
                writeln("*LR*\nSource: ", pat, "\nStack: ",fixupStack.stack.data);
            switch(current)
            {
            case '(':
                next();
                nesting++;
                uint nglob;
                fixupStack.push(cast(uint)ir.length);
                if(current == '?')
                {
                    next();
                    switch(current)
                    {
                    case ':':
                        put(Bytecode(IR.Nop, 0));
                        next();
                        break;
                    case '=':
                        genLookaround(IR.LookaheadStart);
                        next();
                        break;
                    case '!':
                        genLookaround(IR.NeglookaheadStart);
                        next();
                        break;
                    case 'P':
                        next();
                        if(current != '<')
                            error("Expected '<' in named group");
                        string name;
                        while(next() && isAlpha(current))
                        {
                            name ~= current;
                        }
                        if(current != '>')
                            error("Expected '>' closing named group");
                        next();
                        nglob = groupStack.top++;
                        enforce(groupStack.top <= maxGroupNumber, "limit on submatches is exceeded");
                        auto t = NamedGroup(name, nglob);
                        auto d = assumeSorted!"a.name < b.name"(dict);
                        auto ind = d.lowerBound(t).length;
                        insertInPlaceAlt(dict, ind, t);
                        put(Bytecode(IR.GroupStart, nglob));
                        break;
                    case '<':
                        next();
                        if(current == '=')
                            genLookaround(IR.LookbehindStart);
                        else if(current == '!')
                            genLookaround(IR.NeglookbehindStart);
                        else
                            error("'!' or '=' expected after '<'");
                        next();
                        break;
                    default:
                        error(" ':', '=', '<', 'P' or '!' expected after '(?' ");
                    }
                }
                else
                {
                    nglob = groupStack.top++;
                    enforce(groupStack.top <= maxGroupNumber, "limit on number of submatches is exceeded");
                    put(Bytecode(IR.GroupStart, nglob));
                }
                break;
            case ')':
                enforce(nesting, "Unmatched ')'");
                nesting--;
                next();
                fix = fixupStack.pop();
                switch(ir[fix].code)
                {
                case IR.GroupStart:
                    put(Bytecode(IR.GroupEnd,ir[fix].data));
                    parseQuantifier(fix);
                    break;
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                    assert(lookaroundNest);
                    fixLookaround(fix);
                    lookaroundNest--;
                    break;
                case IR.Option: //| xxx )
                    //two fixups: last option + full OR
                    finishAlternation(fix);
                    fix = fixupStack.top;
                    switch(ir[fix].code)
                    {
                    case IR.GroupStart:
                        fixupStack.pop();
                        put(Bytecode(IR.GroupEnd,ir[fix].data));
                        parseQuantifier(fix);
                        break;
                    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                        assert(lookaroundNest);
                        lookaroundNest--;
                        fix = fixupStack.pop();
                        fixLookaround(fix);
                        break;
                    default://(?:xxx)
                        fixupStack.pop();
                        parseQuantifier(fix);
                    }
                    break;
                default://(?:xxx)
                    parseQuantifier(fix);
                }
                break;
            case '|':
                next();
                fix = fixupStack.top;
                if(ir.length > fix && ir[fix].code == IR.Option)
                {
                    ir[fix] = Bytecode(ir[fix].code, cast(uint)ir.length - fix);
                    put(Bytecode(IR.GotoEndOr, 0));
                    fixupStack.top = cast(uint)ir.length; //replace latest fixup for Option
                    put(Bytecode(IR.Option, 0));
                    break;
                }
                uint len, orStart;
                //start a new option
                if(fixupStack.length == 1)
                {//only root entry, effectively no fixup
                    len = cast(uint)ir.length + IRL!(IR.GotoEndOr);
                    orStart = 0;
                }
                else
                {//IR.lookahead, etc. fixups that have length > 1, thus check ir[x].length
                    len = cast(uint)ir.length - fix - (ir[fix].length - 1);
                    orStart = fix + ir[fix].length;
                }
                insertInPlaceAlt(ir, orStart, Bytecode(IR.OrStart, 0), Bytecode(IR.Option, len));
                assert(ir[orStart].code == IR.OrStart);
                put(Bytecode(IR.GotoEndOr, 0));
                fixupStack.push(orStart); //fixup for StartOR
                fixupStack.push(cast(uint)ir.length); //for second Option
                put(Bytecode(IR.Option, 0));
                break;
            default://no groups or whatever
                uint start = cast(uint)ir.length;
                parseAtom();
                parseQuantifier(start);
            }
        }

        if(fixupStack.length != 1)
        {
            fix = fixupStack.pop();
            enforce(ir[fix].code == IR.Option, "no matching ')'");
            finishAlternation(fix);
            enforce(fixupStack.length == 1, "no matching ')'");
        }
    }

    //helper function, finalizes IR.Option, fix points to the first option of sequence
    void finishAlternation(uint fix)
    {
        enforce(ir[fix].code == IR.Option, "no matching ')'");
        ir[fix] = Bytecode(ir[fix].code, cast(uint)ir.length - fix - IRL!(IR.OrStart));
        fix = fixupStack.pop();
        enforce(ir[fix].code == IR.OrStart, "no matching ')'");
        ir[fix] = Bytecode(IR.OrStart, cast(uint)ir.length - fix - IRL!(IR.OrStart));
        put(Bytecode(IR.OrEnd, cast(uint)ir.length - fix - IRL!(IR.OrStart)));
        uint pc = fix + IRL!(IR.OrStart);
        while(ir[pc].code == IR.Option)
        {
            pc = pc + ir[pc].data;
            if(ir[pc].code != IR.GotoEndOr)
                break;
            ir[pc] = Bytecode(IR.GotoEndOr, cast(uint)(ir.length - pc - IRL!(IR.OrEnd)));
            pc += IRL!(IR.GotoEndOr);
        }
        put(Bytecode.fromRaw(0));
    }

    //parse and store IR for atom-quantifier pair
    @trusted void parseQuantifier(uint offset)
    {//copy is @system
        uint replace = ir[offset].code == IR.Nop;
        if(empty && !replace)
            return;
        uint min, max;
        switch(current)
        {
        case '*':
            min = 0;
            max = infinite;
            break;
        case '?':
            min = 0;
            max = 1;
            break;
        case '+':
            min = 1;
            max = infinite;
            break;
        case '{':
            enforce(next(), "Unexpected end of regex pattern");
            enforce(ascii.isDigit(current), "First number required in repetition");
            min = parseDecimal();
            if(current == '}')
                max = min;
            else if(current == ',')
            {
                next();
                if(ascii.isDigit(current))
                    max = parseDecimal();
                else if(current == '}')
                    max = infinite;
                else
                    error("Unexpected symbol in regex pattern");
                skipSpace();
                if(current != '}')
                    error("Unmatched '{' in regex pattern");
            }
            else
                error("Unexpected symbol in regex pattern");
            break;
        default:
            if(replace)
            {
                copyForwardAlt(ir[offset + 1 .. $],ir[offset .. $ - 1]);
                ir.length -= 1;
            }
            return;
        }
        uint len = cast(uint)ir.length - offset - replace;
        bool greedy = true;
        //check only if we managed to get new symbol
        if(next() && current == '?')
        {
            greedy = false;
            next();
        }
        if(max != infinite)
        {
            if(min != 1 || max != 1)
            {
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if(replace)
                    ir[offset] = op;
                else
                    insertInPlaceAlt(ir, offset, op);
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(max);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
        }
        else if(min) //&& max is infinite
        {
            if(min != 1)
            {
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if(replace)
                    ir[offset] = op;
                else
                    insertInPlaceAlt(ir, offset, op);
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth = std.algorithm.max(counterDepth, nesting+1);
            }
            else if(replace)
            {
                copyForwardAlt(ir[offset+1 .. $],ir[offset .. $-1]);
                ir.length -= 1;
            }
            put(Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            enforce(ir.length + len < maxCompiledLength,  "maximum compiled pattern length is exceeded");
            ir ~= ir[offset .. offset+len];
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
        else//vanila {0,inf}
        {
            Bytecode op = Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len);
            if(replace)
                ir[offset] = op;
            else
                insertInPlaceAlt(ir, offset, op);
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index

        }
    }

    //parse and store IR for atom
    void parseAtom()
    {
        if(empty)
            return;
        switch(current)
        {
        case '*', '?', '+', '|', '{', '}':
            error("'*', '+', '?', '{', '}' not allowed in atom");
            break;
        case '.':
            put(Bytecode(IR.Any, 0));
            next();
            break;
        case '[':
            parseCharset();
            break;
        case '\\':
            enforce(_next(), "Unfinished escape sequence");
            parseEscape();
            break;
        case '^':
            put(Bytecode(IR.Bol, 0));
            next();
            break;
        case '$':
            put(Bytecode(IR.Eol, 0));
            next();
            break;
        default:
            if(re_flags & RegexOption.casefold)
            {
                dchar[5] data;
                auto range = getCommonCasing(current, data);
                assert(range.length <= 5);
                if(range.length == 1)
                    put(Bytecode(IR.Char, range[0]));
                else
                    foreach(v; range)
                        put(Bytecode(IR.OrChar, v, cast(uint)range.length));
            }
            else
                put(Bytecode(IR.Char, current));
            next();
        }
    }

    //generate code for start of lookaround: (?= (?! (?<= (?<!
    void genLookaround(IR opcode)
    {
        put(Bytecode(opcode, 0));
        put(Bytecode.fromRaw(0));
        put(Bytecode.fromRaw(0));
        groupStack.push(0);
        lookaroundNest++;
        enforce(lookaroundNest <= maxLookaroundDepth,
            "maximum lookaround depth is exceeded");
    }

    //fixup lookaround with start at offset fix and append a proper *-End opcode
    void fixLookaround(uint fix)
    {
        ir[fix] = Bytecode(ir[fix].code,
            cast(uint)ir.length - fix - IRL!(IR.LookaheadStart));
        auto g = groupStack.pop();
        assert(!groupStack.empty);
        ir[fix+1] = Bytecode.fromRaw(groupStack.top);
        //groups are cumulative across lookarounds
        ir[fix+2] = Bytecode.fromRaw(groupStack.top+g);
        groupStack.top += g;
        if(ir[fix].code == IR.LookbehindStart || ir[fix].code == IR.NeglookbehindStart)
        {            
            reverseBytecode(ir[fix + IRL!(IR.LookbehindStart) .. $]);            
    }
        put(ir[fix].paired);
    }

    //CodepointSet operations relatively in order of priority
    enum Operator:uint {
        Open = 0, Negate,  Difference, SymDifference, Intersection, Union, None
    }

    //parse unit of CodepointSet spec, most notably escape sequences and char ranges
    //also fetches next set operation
    Tuple!(CodepointSet,Operator) parseCharTerm()
    {
        enum State{ Start, Char, Escape, CharDash, CharDashEscape,
            PotentialTwinSymbolOperator }
        Operator op = Operator.None;
        dchar last;
        CodepointSet set;
        State state = State.Start;

        static void addWithFlags(ref CodepointSet set, uint ch, uint re_flags)
        {
            if(re_flags & RegexOption.casefold)
            {
                dchar[5] chars;
                auto range = getCommonCasing(ch, chars);
                foreach(v; range)
                    set.add(v);
            }
            else
                set.add(ch);
        }

        static Operator twinSymbolOperator(dchar symbol)
        {
            switch(symbol)
            {
            case '|':
                return Operator.Union;
            case '-':
                return Operator.Difference;
            case '~':
                return Operator.SymDifference;
            case '&':
                return Operator.Intersection;
            default:
                assert(false);
            }
        }

        L_CharTermLoop:
        for(;;)
        {
            final switch(state)
            {
            case State.Start:
                switch(current)
                {
                case '|':
                case '-':
                case '~':
                case '&':
                    state = State.PotentialTwinSymbolOperator;
                    last = current;
                    break;
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    break L_CharTermLoop;
                case '\\':
                    state = State.Escape;
                    break;
                default:
                    state = State.Char;
                    last = current;
                }
                break;
            case State.Char:
                // xxx last current xxx
                switch(current)
                {
                case '|':
                case '~':
                case '&':
                    // then last is treated as normal char and added as implicit union
                    state = State.PotentialTwinSymbolOperator;
                    addWithFlags(set, last, re_flags);
                    last = current;
                    break;
                case '-': // still need more info
                    state = State.CharDash;
                    break;
                case '\\':
                    set.add(last);
                    state = State.Escape;
                    break;
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    set.add(last);
                    break L_CharTermLoop;
                default:
                    addWithFlags(set, last, re_flags);
                    last = current;
                }
                break;
            case State.PotentialTwinSymbolOperator:
                // xxx last current xxxx
                // where last = [|-&~]
                if(current == last)
                {
                    op = twinSymbolOperator(last);
                    next();//skip second twin char
                    break L_CharTermLoop;
                }
                goto case State.Char;// it's not a twin lets re-run normal logic
            case State.Escape:
                // xxx \ current xxx
                switch(current)
                {
                case 'f':
                    last = '\f';
                    state = State.Char;
                    break;
                case 'n':
                    last = '\n';
                    state = State.Char;
                    break;
                case 'r':
                    last = '\r';
                    state = State.Char;
                    break;
                case 't':
                    last = '\t';
                    state = State.Char;
                    break;
                case 'v':
                    last = '\v';
                    state = State.Char;
                    break;
                case 'c':
                    last = parseControlCode();
                    state = State.Char;
                    break;
                foreach(val; Escapables)
                {
                case val:
                }
                    last = current;
                    state = State.Char;
                    break;
                case 'p':
                    set.add(parseUnicodePropertySpec(false));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                case 'P':
                    set.add(parseUnicodePropertySpec(true));
                    state = State.Start;
                    continue L_CharTermLoop; //next char already fetched
                case 'x':
                    last = parseUniHex(pat, 2);
                    state = State.Char;
                    break;
                case 'u':
                    last = parseUniHex(pat, 4);
                    state = State.Char;
                    break;
                case 'U':
                    last = parseUniHex(pat, 8);
                    state = State.Char;
                    break;
                case 'd':
                    set.add(unicodeNd);
                    state = State.Start;
                    break;
                case 'D':
                    set.add(unicodeNd.dup.negate());
                    state = State.Start;
                    break;
                case 's':
                    set.add(unicodeWhite_Space);
                    state = State.Start;
                    break;
                case 'S':
                    set.add(unicodeWhite_Space.dup.negate());
                    state = State.Start;
                    break;
                case 'w':
                    set.add(wordCharacter);
                    state = State.Start;
                    break;
                case 'W':
                    set.add(wordCharacter.dup.negate());
                    state = State.Start;
                    break;
                default:
                    enforce(false, "invalid escape sequence");
                }
                break;
            case State.CharDash:
                // xxx last - current xxx
                switch(current)
                {
                case '[':
                    op = Operator.Union;
                    goto case;
                case ']':
                    //means dash is a single char not an interval specifier
                    addWithFlags(set, last, re_flags);
                    addWithFlags(set, '-', re_flags);
                    break L_CharTermLoop;
                 case '-'://set Difference again
                    addWithFlags(set, last, re_flags);
                    op = Operator.Difference;
                    next();//skip '-'
                    break L_CharTermLoop;
                case '\\':
                    state = State.CharDashEscape;
                    break;
                default:
                    enforce(last <= current, "inverted range");
                    if(re_flags & RegexOption.casefold)
                    {
                        for(uint ch = last; ch <= current; ch++)
                            addWithFlags(set, ch, re_flags);
                    }
                    else
                        set.add(Interval(last, current));
                    state = State.Start;
                }
                break;
            case State.CharDashEscape:
            //xxx last - \ current xxx
                uint end;
                switch(current)
                {
                case 'f':
                    end = '\f';
                    break;
                case 'n':
                    end = '\n';
                    break;
                case 'r':
                    end = '\r';
                    break;
                case 't':
                    end = '\t';
                    break;
                case 'v':
                    end = '\v';
                    break;
                foreach(val; Escapables)
                {
                case val:
                }
                    end = current;
                    break;
                case 'c':
                    end = parseControlCode();
                    break;
                case 'x':
                    end = parseUniHex(pat, 2);
                    break;
                case 'u':
                    end = parseUniHex(pat, 4);
                    break;
                case 'U':
                    end = parseUniHex(pat, 8);
                    break;
                default:
                    error("invalid escape sequence");
                }
                enforce(last <= end,"inverted range");
                set.add(Interval(last,end));
                state = State.Start;
                break;
            }
            enforce(next(), "unexpected end of CodepointSet");
        }
        return tuple(set, op);
    }

    alias ValStack = Stack!(CodepointSet);
    alias OpStack = Stack!(Operator);

    //parse and store IR for CodepointSet
    void parseCharset()
    {
        ValStack vstack;
        OpStack opstack;
        //
        static bool apply(Operator op, ref ValStack stack)
        {
            switch(op)
            {
            case Operator.Negate:
                stack.top.negate();
                break;
            case Operator.Union:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '||'");
                stack.top.add(s);
                break;
            case Operator.Difference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '--'");
                stack.top.sub(s);
                break;
            case Operator.SymDifference:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '~~'");
                stack.top.symmetricSub(s);
                break;
            case Operator.Intersection:
                auto s = stack.pop();//2nd operand
                enforce(!stack.empty, "no operand for '&&'");
                stack.top.intersect(s);
                break;
            default:
                return false;
            }
            return true;
        }
        static bool unrollWhile(alias cond)(ref ValStack vstack, ref OpStack opstack)
        {
            while(cond(opstack.top))
            {
                if(!apply(opstack.pop(),vstack))
                    return false;//syntax error
                if(opstack.empty)
                    return false;
            }
            return true;
        }

        L_CharsetLoop:
        do
        {
            switch(current)
            {
            case '[':
                opstack.push(Operator.Open);
                enforce(next(), "unexpected end of character class");
                if(current == '^')
                {
                    opstack.push(Operator.Negate);
                    enforce(next(), "unexpected end of character class");
                }
                //[] is prohibited
                enforce(current != ']', "wrong character class");
                goto default;
            case ']':
                enforce(unrollWhile!(unaryFun!"a != a.Open")(vstack, opstack),
                    "character class syntax error");
                enforce(!opstack.empty, "unmatched ']'");
                opstack.pop();
                next();
                if(opstack.empty)
                    break L_CharsetLoop;
                auto pair  = parseCharTerm();
                if(!pair[0].empty)//not only operator e.g. -- or ~~
                {
                    vstack.top.add(pair[0]);//apply union
                }
                if(pair[1] != Operator.None)
                {
                    if(opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                break;
            //
            default://yet another pair of term(op)?
                auto pair = parseCharTerm();
                if(pair[1] != Operator.None)
                {
                    if(opstack.top == Operator.Union)
                        unrollWhile!(unaryFun!"a == a.Union")(vstack, opstack);
                    opstack.push(pair[1]);
                }
                vstack.push(pair[0]);
            }

        }while(!empty || !opstack.empty);
        while(!opstack.empty)
            apply(opstack.pop(),vstack);
        assert(vstack.length == 1);
        charsetToIr(vstack.top);
    }
    //try to generate optimal IR code for this CodepointSet
    @trusted void charsetToIr(in CodepointSet set)
    {//@@@BUG@@@ writeln is @system
        uint chars = set.chars;
        if(chars < Bytecode.maxSequence)
        {
            switch(chars)
            {
                case 1:
                    put(Bytecode(IR.Char, set.ivals[0]));
                    break;
                case 0:
                    error("empty CodepointSet not allowed");
                    break;
                default:
                    foreach(ch; set[])
                        put(Bytecode(IR.OrChar, ch, chars));
            }
        }
        else
        {
            if(set.ivals.length > maxCharsetUsed)
            {
                auto t  = getTrie(set);
                put(Bytecode(IR.Trie, cast(uint)tries.length));
                tries ~= t;
                debug(std_regex_allocation) writeln("Trie generated");
            }
            else
            {
                put(Bytecode(IR.CodepointSet, cast(uint)charsets.length));
                tries ~= Trie.init;
            }
            charsets ~= set;
            assert(charsets.length == tries.length);
        }
    }

    //parse and generate IR for escape stand alone escape sequence
    @trusted void parseEscape()
    {//accesses array of appender

        switch(current)
        {
        case 'f':   next(); put(Bytecode(IR.Char, '\f')); break;
        case 'n':   next(); put(Bytecode(IR.Char, '\n')); break;
        case 'r':   next(); put(Bytecode(IR.Char, '\r')); break;
        case 't':   next(); put(Bytecode(IR.Char, '\t')); break;
        case 'v':   next(); put(Bytecode(IR.Char, '\v')); break;

        case 'd':
            next();
            charsetToIr(unicodeNd);
            break;
        case 'D':
            next();
            charsetToIr(unicodeNd.dup.negate());
            break;
        case 'b':   next(); put(Bytecode(IR.Wordboundary, 0)); break;
        case 'B':   next(); put(Bytecode(IR.Notwordboundary, 0)); break;
        case 's':
            next();
            charsetToIr(unicodeWhite_Space);
            break;
        case 'S':
            next();
            charsetToIr(unicodeWhite_Space.dup.negate());
            break;
        case 'w':
            next();
            charsetToIr(wordCharacter);
            break;
        case 'W':
            next();
            charsetToIr(wordCharacter.dup.negate());
            break;
        case 'p': case 'P':
            auto CodepointSet = parseUnicodePropertySpec(current == 'P');
            charsetToIr(CodepointSet);
            break;
        case 'x':
            uint code = parseUniHex(pat, 2);
            next();
            put(Bytecode(IR.Char,code));
            break;
        case 'u': case 'U':
            uint code = parseUniHex(pat, current == 'u' ? 4 : 8);
            next();
            put(Bytecode(IR.Char, code));
            break;
        case 'c': //control codes
            Bytecode code = Bytecode(IR.Char, parseControlCode());
            next();
            put(code);
            break;
        case '0':
            next();
            put(Bytecode(IR.Char, 0));//NUL character
            break;
        case '1': .. case '9':
            uint nref = cast(uint)current - '0';
            uint maxBackref;
            foreach(v; groupStack.data)
                maxBackref += v;
            uint localLimit = maxBackref - groupStack.top;
            enforce(nref < maxBackref, "Backref to unseen group");
            //perl's disambiguation rule i.e.
            //get next digit only if there is such group number
            while(nref < maxBackref && next() && ascii.isDigit(current))
            {
                nref = nref * 10 + current - '0';
            }
            if(nref >= maxBackref)
                nref /= 10;

            if(nref >= localLimit)
            {
                put(Bytecode(IR.Backref, nref-localLimit));
                ir[$-1].setLocalRef();
            }
            else
                put(Bytecode(IR.Backref, nref));
            markBackref(nref);
            break;
        default:
            auto op = Bytecode(IR.Char, current);
            next();
            put(op);
        }
    }

    //parse and return a CodepointSet for \p{...Property...} and \P{...Property..},
    //\ - assumed to be processed, p - is current
    const(CodepointSet) parseUnicodePropertySpec(bool negated)
    {
        alias ucmp = comparePropertyName;
        enum MAX_PROPERTY = 128;
        char[MAX_PROPERTY] result;
        uint k = 0;
        enforce(next());
        if(current == '{')
        {
            while(k < MAX_PROPERTY && next() && current !='}' && current !=':')
                if(current != '-' && current != ' ' && current != '_')
                    result[k++] = cast(char)ascii.toLower(current);
            enforce(k != MAX_PROPERTY, "invalid property name");
            enforce(current == '}', "} expected ");
        }
        else
        {//single char properties e.g.: \pL, \pN ...
            enforce(current < 0x80, "invalid property name");
            result[k++] = cast(char)current;
        }
        auto s = getUnicodeSet(result[0..k], negated,
            cast(bool)(re_flags & RegexOption.casefold));
        enforce(!s.empty, "unrecognized unicode property spec");
        next();
        return s;
    }

    //
    @trusted void error(string msg)
    {
        auto app = appender!string();
        ir = null;
        formattedWrite(app, "%s\nPattern with error: `%s` <--HERE-- `%s`",
                       msg, origin[0..$-pat.length], pat);
        throw new RegexException(app.data);
    }

    alias Char = BasicElementOf!R;
    //packages parsing results into a RegEx object
    @property Regex!Char program()
    {
        return Regex!Char(this);
    }
}


/++
    $(D Regex) object holds regular expression pattern in compiled form.
    Instances of this object are constructed via calls to $(D regex).
    This is an intended form for caching and storage of frequently
    used regular expressions.
+/
public struct Regex(Char)
{
    //temporary workaround for identifier lookup
    const(CodepointSet)[] charsets; //
    Bytecode[] ir;      //compiled bytecode of pattern

    /++
        Test if this object doesn't contain any compiled pattern.
        Example:
        ---
        Regex!char r;
        assert(r.empty);
        r = regex(""); // Note: "" is a valid regex pattern.
        assert(!r.empty);
        ---
    +/
    @safe @property bool empty() const nothrow {  return ir is null; }

    /++
        A range of all the named captures in the regex.
        Example:
        ----
        import std.range;
        import std.algorithm;

        auto re = regex(`(?P<name>\w+) = (?P<var>\d+)`);
        auto nc = re.namedCaptures;
        static assert(isRandomAccessRange!(typeof(nc)));
        assert(!nc.empty);
        assert(nc.length == 2);
        assert(nc.equal(["name", "var"]));
        assert(nc[0] == "name");
        assert(nc[1..$].equal(["var"]));
        ----
     +/
    @safe @property auto namedCaptures()
    {
        static struct NamedGroupRange
        {
        private:
            NamedGroup[] groups;
            size_t start;
            size_t end;
        public:
            this(NamedGroup[] g, size_t s, size_t e)
            {
                assert(s <= e);
                assert(e <= g.length);
                groups = g;
                start = s;
                end = e;
            }

            @property string front() { return groups[start].name; }
            @property string back() { return groups[end-1].name; }
            @property bool empty() { return start >= end; }
            @property size_t length() { return end - start; }
            alias opDollar = length;
            @property NamedGroupRange save()
            {
                return NamedGroupRange(groups, start, end);
            }
            void popFront() { assert(!empty); start++; }
            void popBack() { assert(!empty); end--; }
            string opIndex()(size_t i)
            {
                assert(start + i < end,
                       "Requested named group is out of range.");
                return groups[start+i].name;
            }
            NamedGroupRange opSlice(size_t low, size_t high) {
                assert(low <= high);
                assert(start + high <= end);
                return NamedGroupRange(groups, start + low, start + high);
            }
            NamedGroupRange opSlice() { return this.save; }
        }
        return NamedGroupRange(dict, 0, dict.length);
    }

private:
    NamedGroup[] dict;  //maps name -> user group number
    uint ngroup;        //number of internal groups
    uint maxCounterDepth; //max depth of nested {n,m} repetitions
    uint hotspotTableSize; //number of entries in merge table
    uint threadCount;
    uint flags;         //global regex flags
    const(Trie)[]  tries; //
    uint[] backrefed; //bit array of backreferenced submatches
    Kickstart!Char kickstart;

    //bit access helper
    uint isBackref(uint n)
    {
        if(n/32 >= backrefed.length)
            return 0;
        return backrefed[n / 32] & (1 << (n & 31));
    }

    //check if searching is not needed
    void checkIfOneShot()
    {
        if(flags & RegexOption.multiline)
            return;
    L_CheckLoop:
        for(uint i = 0; i < ir.length; i += ir[i].length)
        {
            switch(ir[i].code)
            {
                case IR.Bol:
                    flags |= RegexInfo.oneShot;
                    break L_CheckLoop;
                case IR.GroupStart, IR.GroupEnd, IR.Eol, IR.Wordboundary, IR.Notwordboundary:
                    break;
                default:
                    break L_CheckLoop;
            }
        }
    }

    /+
        lightweight post process step,
        only essentials
    +/
    @trusted void lightPostprocess()
    {//@@@BUG@@@ write is @system
        struct FixedStack(T)
        {
            T[] arr;
            uint _top;
            //this(T[] storage){   arr = storage; _top = -1; }
            @property ref T top(){  assert(!empty); return arr[_top]; }
            void push(T x){  arr[++_top] = x; }
            T pop() { assert(!empty);   return arr[_top--]; }
            @property bool empty(){   return _top == -1; }
        }
        auto counterRange = FixedStack!uint(new uint[maxCounterDepth+1], -1);
        counterRange.push(1);
        ulong cumRange = 0;
        for(uint i = 0; i < ir.length; i += ir[i].length)
        {
            if(ir[i].hotspot)
            {
                assert(i + 1 < ir.length,
                    "unexpected end of IR while looking for hotspot");
                ir[i+1] = Bytecode.fromRaw(hotspotTableSize);
                hotspotTableSize += counterRange.top;
            }
            switch(ir[i].code)
            {
            case IR.RepeatStart, IR.RepeatQStart:
                uint repEnd = cast(uint)(i + ir[i].data + IRL!(IR.RepeatStart));
                assert(ir[repEnd].code == ir[i].paired.code);
                uint max = ir[repEnd + 4].raw;
                ir[repEnd+2].raw = counterRange.top;
                ir[repEnd+3].raw *= counterRange.top;
                ir[repEnd+4].raw *= counterRange.top;
                ulong cntRange = cast(ulong)(max)*counterRange.top;
                cumRange += cntRange;
                enforce(cumRange < maxCumulativeRepetitionLength,
                    "repetition length limit is exceeded");
                counterRange.push(cast(uint)cntRange + counterRange.top);
                threadCount += counterRange.top;
                break;
            case IR.RepeatEnd, IR.RepeatQEnd:
                threadCount += counterRange.top;
                counterRange.pop();
                break;
            case IR.GroupStart:
                if(isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            case IR.GroupEnd:
                if(isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            default:
                threadCount += counterRange.top;
            }
        }
        checkIfOneShot();
        if(!(flags & RegexInfo.oneShot))
            kickstart = Kickstart!Char(this, new uint[](256));
        debug(std_regex_allocation) writefln("IR processed, max threads: %d", threadCount);
    }

    //IR code validator - proper nesting, illegal instructions, etc.
    @trusted void validate()
    {//@@@BUG@@@ text is @system
        for(uint pc = 0; pc < ir.length; pc += ir[pc].length)
        {
            if(ir[pc].isStart || ir[pc].isEnd)
            {
                uint dest = ir[pc].indexOfPair(pc);
                assert(dest < ir.length, text("Wrong length in opcode at pc=",
                    pc, " ", dest, " vs ", ir.length));
                assert(ir[dest].paired ==  ir[pc],
                    text("Wrong pairing of opcodes at pc=", pc, "and pc=", dest));
            }
            else if(ir[pc].isAtom)
            {

            }
            else
               assert(0, text("Unknown type of instruction at pc=", pc));
        }
    }

    //print out disassembly a program's IR
    @trusted debug(std_regex_parser) void print() const
    {//@@@BUG@@@ write is system
        for(uint i = 0; i < ir.length; i += ir[i].length)
        {
            writefln("%d\t%s ", i, disassemble(ir, i, dict));
        }
        writeln("Total merge table size: ", hotspotTableSize);
        writeln("Max counter nesting depth: ", maxCounterDepth);
    }

    //
    this(S)(Parser!(S) p)
    {
        ir = p.ir;
        dict = p.dict;
        ngroup = p.groupStack.top;
        maxCounterDepth = p.counterDepth;
        flags = p.re_flags;
        charsets = p.charsets;
        tries = p.tries;
        backrefed = p.backrefed;
        lightPostprocess();
        debug(std_regex_parser)
        {
            print();
        }
        version(assert) validate();
    }
}

unittest
{
    auto re = regex(`(?P<name>\w+) = (?P<var>\d+)`);
    auto nc = re.namedCaptures;
    static assert(isRandomAccessRange!(typeof(nc)));
    assert(!nc.empty);
    assert(nc.length == 2);
    assert(nc.equal(["name", "var"]));
    assert(nc[0] == "name");
    assert(nc[1..$].equal(["var"]));

    re = regex(`(\w+) (?P<named>\w+) (\w+)`);
    nc = re.namedCaptures;
    assert(nc.length == 1);
    assert(nc[0] == "named");
    assert(nc.front == "named");
    assert(nc.back == "named");

    re = regex(`(\w+) (\w+)`);
    nc = re.namedCaptures;
    assert(nc.empty);

    re = regex(`(?P<year>\d{4})/(?P<month>\d{2})/(?P<day>\d{2})/`);
    nc = re.namedCaptures;
    auto cp = nc.save;
    assert(nc.equal(cp));
    nc.popFront();
    assert(nc.equal(cp[1..$]));
    nc.popBack();
    assert(nc.equal(cp[1 .. $ - 1]));
}

//
@trusted uint lookupNamedGroup(String)(NamedGroup[] dict, String name)
{//equal is @system?
    auto fnd = assumeSorted!"cmp(a,b) < 0"(map!"a.name"(dict)).lowerBound(name).length;
    enforce(equal(dict[fnd].name, name), text("no submatch named ", name));
    return dict[fnd].group;
}

//whether ch is one of unicode newline sequences
bool endOfLine(dchar front, bool seenCr)
{
    return ((front == '\n') ^ seenCr) || front == '\r'
    || front == NEL || front == LS || front == PS;
}

//
bool startOfLine(dchar back, bool seenNl)
{
    return ((back == '\r') ^ seenNl) || back == '\n'
    || back == NEL || back == LS || back == PS;
}

//Test if bytecode starting at pc in program 're' can match given codepoint
//Returns: 0 - can't tell, -1 if doesn't match
int quickTestFwd(RegEx)(uint pc, dchar front, const ref RegEx re)
{
    static assert(IRL!(IR.OrChar) == 1);//used in code processing IR.OrChar
    for(;;)
        switch(re.ir[pc].code)
        {
        case IR.OrChar:
            uint len = re.ir[pc].sequence;
            uint end = pc + len;
            if(re.ir[pc].data != front && re.ir[pc+1].data != front)
            {
                for(pc = pc+2; pc < end; pc++)
                    if(re.ir[pc].data == front)
                        break;
                if(pc == end)
                    return -1;
            }
            return 0;
        case IR.Char:
            if(front == re.ir[pc].data)
                return 0;
            else
                return -1;
        case IR.Any:
            return 0;
        case IR.CodepointSet:
            if(re.charsets[re.ir[pc].data].scanFor(front))
                return 0;
            else
                return -1;
        case IR.GroupStart, IR.GroupEnd:
            pc += IRL!(IR.GroupStart);
            break;
        case IR.Trie:
            if(re.tries[re.ir[pc].data][front])
                return 0;
            else
                return -1;
        default:
            return 0;
        }
}


//unsafe, no initialization of elements
@system T[] mallocArray(T)(size_t len)
{
    return (cast(T*)malloc(len * T.sizeof))[0 .. len];
}

//very unsafe, no initialization
@system T[] arrayInChunk(T)(size_t len, ref void[] chunk)
{
    auto ret = (cast(T*)chunk.ptr)[0..len];
    chunk = chunk[len * T.sizeof .. $];
    return ret;
}

/++
    A $(D StaticRegex) is $(D Regex) object that contains specially
    generated machine code to speed up matching.
    Implicitly convertible to normal $(D Regex),
    however doing so will result in losing this additional capability.
+/
public struct StaticRegex(Char)
{
private:
    alias Matcher = BacktrackingMatcher!(true);
    alias MatchFn = bool function(ref Matcher!Char) @trusted;
    MatchFn nativeFn;
public:
    Regex!Char _regex;
    alias _regex this;
    this(Regex!Char re, MatchFn fn)
    {
        _regex = re;
        nativeFn = fn;

    }

}

//utility for shiftOr, returns a minimum number of bytes to test in a Char
uint effectiveSize(Char)()
{
    static if(is(Char == char))
        return 1;
    else static if(is(Char == wchar))
        return 2;
    else static if(is(Char == dchar))
        return 3;
    else
        static assert(0);
}

/*
    Kickstart engine using ShiftOr algorithm,
    a bit parallel technique for inexact string searching.
*/
struct ShiftOr(Char)
{
private:
    uint[] table;
    uint fChar;
    uint n_length;
    enum charSize =  effectiveSize!Char();
    //maximum number of chars in CodepointSet to process
    enum uint charsetThreshold = 32_000;
    static struct ShiftThread
    {
        uint[] tab;
        uint mask;
        uint idx;
        uint pc, counter, hops;
        this(uint newPc, uint newCounter, uint[] table)
        {
            pc = newPc;
            counter = newCounter;
            mask = 1;
            idx = 0;
            hops = 0;
            tab = table;
        }

        void setMask(uint idx, uint mask)
        {
            tab[idx] |= mask;
        }

        void setInvMask(uint idx, uint mask)
        {
            tab[idx] &= ~mask;
        }

        void set(alias setBits = setInvMask)(dchar ch)
        {
            static if(charSize == 3)
            {
                uint val = ch, tmask = mask;
                setBits(val&0xFF, tmask);
                tmask <<= 1;
                val >>= 8;
                setBits(val&0xFF, tmask);
                tmask <<= 1;
                val >>= 8;
                assert(val <= 0x10);
                setBits(val, tmask);
                tmask <<= 1;
            }
            else
            {
                Char[dchar.sizeof/Char.sizeof] buf;
                uint tmask = mask;
                size_t total = encode(buf, ch);
                for(size_t i = 0; i < total; i++, tmask<<=1)
                {
                    static if(charSize == 1)
                        setBits(buf[i], tmask);
                    else static if(charSize == 2)
                    {
                        setBits(buf[i]&0xFF, tmask);
                        tmask <<= 1;
                        setBits(buf[i]>>8, tmask);
                    }
                }
            }
        }
        void add(dchar ch){ return set!setInvMask(ch); }
        void advance(uint s)
        {
            mask <<= s;
            idx += s;
        }
        @property bool full(){    return !mask; }
    }

    static ShiftThread fork(ShiftThread t, uint newPc, uint newCounter)
    {
        ShiftThread nt = t;
        nt.pc = newPc;
        nt.counter = newCounter;
        return nt;
    }

    @trusted static ShiftThread fetch(ref ShiftThread[] worklist)
    {
        auto t = worklist[$-1];
        worklist.length -= 1;
        if(!__ctfe)
            worklist.assumeSafeAppend();
        return t;
    }

    static uint charLen(uint ch)
    {
        assert(ch <= 0x10FFFF);
        return codeLength!Char(cast(dchar)ch)*charSize;
    }

public:
    @trusted this(const ref Regex!Char re, uint[] memory)
    {
        assert(memory.length == 256);
        fChar = uint.max;
    L_FindChar:
        for(size_t i = 0;;)
        {
            switch(re.ir[i].code)
            {
                case IR.Char:
                    fChar = re.ir[i].data;
                    static if(charSize != 3)
                    {
                        Char[dchar.sizeof/Char.sizeof] buf;
                        encode(buf, fChar);
                        fChar = buf[0];
                    }
                    fChar = fChar & 0xFF;
                    break L_FindChar;
                case IR.GroupStart, IR.GroupEnd:
                    i += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                    i += IRL!(IR.Bol);
                    break;
                default:
                    break L_FindChar;
            }
        }
        table = memory;
        table[] =  uint.max;
        ShiftThread[] trs;
        ShiftThread t = ShiftThread(0, 0, table);
        //locate first fixed char if any
        n_length = 32;
        for(;;)
        {
        L_Eval_Thread:
            for(;;)
            {
                switch(re.ir[t.pc].code)
                {
                case IR.Char:
                    uint s = charLen(re.ir[t.pc].data);
                    if(t.idx+s > n_length)
                        goto L_StopThread;
                    t.add(re.ir[t.pc].data);
                    t.advance(s);
                    t.pc += IRL!(IR.Char);
                    break;
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    uint len = re.ir[t.pc].sequence;
                    uint end = t.pc + len;
                    uint[Bytecode.maxSequence] s;
                    uint numS;
                    for(uint i = 0; i < len; i++)
                    {
                        auto x = charLen(re.ir[t.pc+i].data);
                        if(countUntil(s[0..numS], x) < 0)
                           s[numS++] = x;
                    }
                    for(uint i = t.pc; i < end; i++)
                    {
                        t.add(re.ir[i].data);
                    }
                    for(uint i = 0; i < numS; i++)
                    {
                        auto tx = fork(t, t.pc + len, t.counter);
                        if(tx.idx + s[i] <= n_length)
                        {
                            tx.advance(s[i]);
                            trs ~= tx;
                        }
                    }
                    if(!trs.empty)
                        t = fetch(trs);
                    else
                        goto L_StopThread;
                    break;
                case IR.CodepointSet:
                case IR.Trie:
                    auto set = re.charsets[re.ir[t.pc].data];
                    uint[4] s;
                    uint numS;
                    static if(charSize == 3)
                    {
                        s[0] = charSize;
                        numS = 1;
                    }
                    else
                    {
                        static if(charSize == 1)
                            static immutable codeBounds = [0x0, 0x7F, 0x80, 0x7FF, 0x800, 0xFFFF, 0x10000, 0x10FFFF];
                        else //== 2
                            static immutable codeBounds = [0x0, 0xFFFF, 0x10000, 0x10FFFF];
                        auto srange = assumeSorted!"a <= b"(set.ivals);
                        for(uint i = 0; i < codeBounds.length/2; i++)
                        {
                            auto start = srange.lowerBound(codeBounds[2*i]).length;
                            auto end = srange.lowerBound(codeBounds[2*i+1]).length;
                            if(end > start || (end == start && (end & 1)))
                               s[numS++] = (i+1)*charSize;
                        }
                    }
                    if(numS == 0 || t.idx + s[numS-1] > n_length)
                        goto L_StopThread;
                    auto  chars = set.chars;
                    if(chars > charsetThreshold)
                        goto L_StopThread;
                    foreach(ch; set[])
                    {
                        //avoid surrogate pairs
                        if(0xD800 <= ch && ch <= 0xDFFF)
                            continue;
                        t.add(ch);
                    }
                    for(uint i = 0; i < numS; i++)
                    {
                        auto tx =  fork(t, t.pc + IRL!(IR.CodepointSet), t.counter);
                        tx.advance(s[i]);
                        trs ~= tx;
                    }
                    if(!trs.empty)
                        t = fetch(trs);
                    else
                        goto L_StopThread;
                    break;
                case IR.Any:
                    goto L_StopThread;

                case IR.GotoEndOr:
                    t.pc += IRL!(IR.GotoEndOr)+re.ir[t.pc].data;
                    assert(re.ir[t.pc].code == IR.OrEnd);
                    goto case;
                case IR.OrEnd:
                    t.pc += IRL!(IR.OrEnd);
                    break;
                case IR.OrStart:
                    t.pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    uint next = t.pc + re.ir[t.pc].data + IRL!(IR.Option);
                    //queue next Option
                    if(re.ir[next].code == IR.Option)
                    {
                        trs ~= fork(t, next, t.counter);
                    }
                    t.pc += IRL!(IR.Option);
                    break;
                case IR.RepeatStart:case IR.RepeatQStart:
                    t.pc += IRL!(IR.RepeatStart)+re.ir[t.pc].data;
                    goto case IR.RepeatEnd;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    uint len = re.ir[t.pc].data;
                    uint step = re.ir[t.pc+2].raw;
                    uint min = re.ir[t.pc+3].raw;
                    if(t.counter < min)
                    {
                        t.counter += step;
                        t.pc -= len;
                        break;
                    }
                    uint max = re.ir[t.pc+4].raw;
                    if(t.counter < max)
                    {
                        trs ~= fork(t, t.pc - len, t.counter + step);
                        t.counter = t.counter%step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                    else
                    {
                        t.counter = t.counter%step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    t.pc += re.ir[t.pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; //both Q and non-Q
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    uint len = re.ir[t.pc].data;
                    uint pc1, pc2; //branches to take in priority order
                    if(++t.hops == 32)
                        goto L_StopThread;
                    pc1 = t.pc + IRL!(IR.InfiniteEnd);
                    pc2 = t.pc - len;
                    trs ~= fork(t, pc2, t.counter);
                    t.pc = pc1;
                    break;
                case IR.GroupStart, IR.GroupEnd:
                    t.pc += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                    t.pc += IRL!(IR.Bol);
                    break;
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                    t.pc += IRL!(IR.LookaheadStart) + IRL!(IR.LookaheadEnd) + re.ir[t.pc].data;
                    break;
                default:
                L_StopThread:
                    assert(re.ir[t.pc].code >= 0x80, text(re.ir[t.pc].code));
                    debug (fred_search) writeln("ShiftOr stumbled on ",re.ir[t.pc].mnemonic);
                    n_length = min(t.idx, n_length);
                    break L_Eval_Thread;
                }
            }
            if(trs.empty)
                break;
            t = fetch(trs);
        }
        debug(std_regex_search)
        {
            writeln("Min length: ", n_length);
        }
    }

    @property bool empty() const {  return n_length == 0; }

    @property uint length() const{ return n_length/charSize; }

    // lookup compatible bit pattern in haystack, return starting index
    // has a useful trait: if supplied with valid UTF indexes,
    // returns only valid UTF indexes
    // (that given the haystack in question is valid UTF string)
    @trusted size_t search(const(Char)[] haystack, size_t idx)
    {//@BUG: apparently assumes little endian machines
        assert(!empty);
        auto p = cast(const(ubyte)*)(haystack.ptr+idx);
        uint state = uint.max;
        uint limit = 1u<<(n_length - 1u);
        debug(std_regex_search) writefln("Limit: %32b",limit);
        if(fChar != uint.max)
        {
            const(ubyte)* end = cast(ubyte*)(haystack.ptr + haystack.length);
            const orginalAlign = cast(size_t)p & (Char.sizeof-1);
            while(p != end)
            {
                if(!~state)
                {//speed up seeking first matching place
                    for(;;)
                    {
                        assert(p <= end, text(p," vs ", end));
                        p = cast(ubyte*)memchr(p, fChar, end - p);
                        if(!p)
                            return haystack.length;
                        if((cast(size_t)p & (Char.sizeof-1)) == orginalAlign)
                            break;
                        if(++p == end)
                            return haystack.length;
                    }
                    state = ~1u;
                    assert((cast(size_t)p & (Char.sizeof-1)) == orginalAlign);
                    static if(charSize == 3)
                    {
                        state = (state<<1) | table[p[1]];
                        state = (state<<1) | table[p[2]];
                        p += 4;
                    }
                    else
                        p++;
                    //first char is tested, see if that's all
                    if(!(state & limit))
                        return (p-cast(ubyte*)haystack.ptr)/Char.sizeof
                            -length;
                }
                else
                {//have some bits/states for possible matches,
                 //use the usual shift-or cycle
                    static if(charSize == 3)
                    {
                        state = (state<<1) | table[p[0]];
                        state = (state<<1) | table[p[1]];
                        state = (state<<1) | table[p[2]];
                        p += 4;
                    }
                    else
                    {
                        state = (state<<1) | table[p[0]];
                        p++;
                    }
                    if(!(state & limit))
                        return (p-cast(ubyte*)haystack.ptr)/Char.sizeof
                            -length;
                }
                debug(std_regex_search) writefln("State: %32b", state);
            }
        }
        else
        {
            //normal path, partially unrolled for char/wchar
            static if(charSize == 3)
            {
                const(ubyte)* end = cast(ubyte*)(haystack.ptr + haystack.length);
                while(p != end)
                {
                    state = (state<<1) | table[p[0]];
                    state = (state<<1) | table[p[1]];
                    state = (state<<1) | table[p[2]];
                    p += 4;
                    if(!(state & limit))//division rounds down for dchar
                        return (p-cast(ubyte*)haystack.ptr)/Char.sizeof
                        -length;
                }
            }
            else
            {
                auto len = cast(ubyte*)(haystack.ptr + haystack.length) - p;
                size_t i  = 0;
                if(len & 1)
                {
                    state = (state<<1) | table[p[i++]];
                    if(!(state & limit))
                        return idx+i/Char.sizeof-length;
                }
                while(i < len)
                {
                    state = (state<<1) | table[p[i++]];
                    if(!(state & limit))
                        return idx+i/Char.sizeof
                            -length;
                    state = (state<<1) | table[p[i++]];
                    if(!(state & limit))
                        return idx+i/Char.sizeof
                            -length;
                    debug(std_regex_search) writefln("State: %32b", state);
                }
            }
        }
        return haystack.length;
    }

    @system debug static void dump(uint[] table)
    {//@@@BUG@@@ writef(ln) is @system
        import std.stdio;
        for(size_t i = 0; i < table.length; i += 4)
        {
            writefln("%32b %32b %32b %32b",table[i], table[i+1], table[i+2], table[i+3]);
        }
    }
}

unittest
{

    @trusted void test_fixed(alias Kick)()
    {
        foreach(i, v; TypeTuple!(char, wchar, dchar))
        {
            alias Char = v;
            alias String = immutable(v)[];
            auto r = regex(to!String(`abc$`));
            auto kick = Kick!Char(r, new uint[256]);
            assert(kick.length == 3, text(Kick.stringof," ",v.stringof, " == ", kick.length));
            auto r2 = regex(to!String(`(abc){2}a+`));
            kick = Kick!Char(r2, new uint[256]);
            assert(kick.length == 7, text(Kick.stringof,v.stringof," == ", kick.length));
            auto r3 = regex(to!String(`\b(a{2}b{3}){2,4}`));
            kick = Kick!Char(r3, new uint[256]);
            assert(kick.length == 10, text(Kick.stringof,v.stringof," == ", kick.length));
            auto r4 = regex(to!String(`\ba{2}c\bxyz`));
            kick = Kick!Char(r4, new uint[256]);
            assert(kick.length == 6, text(Kick.stringof,v.stringof, " == ", kick.length));
            auto r5 = regex(to!String(`\ba{2}c\b`));
            kick = Kick!Char(r5, new uint[256]);
            size_t x = kick.search("aabaacaa", 0);
            assert(x == 3, text(Kick.stringof,v.stringof," == ", kick.length));
            x = kick.search("aabaacaa", x+1);
            assert(x == 8, text(Kick.stringof,v.stringof," == ", kick.length));
        }
    }
    @trusted void test_flex(alias Kick)()
    {
        foreach(i, v;TypeTuple!(char, wchar, dchar))
        {
            alias Char = v;
            alias String = immutable(v)[];
            auto r = regex(to!String(`abc[a-z]`));
            auto kick = Kick!Char(r, new uint[256]);
            auto x = kick.search(to!String("abbabca"), 0);
            assert(x == 3, text("real x is ", x, " ",v.stringof));

            auto r2 = regex(to!String(`(ax|bd|cdy)`));
            String s2 = to!String("abdcdyabax");
            kick = Kick!Char(r2, new uint[256]);
            x = kick.search(s2, 0);
            assert(x == 1, text("real x is ", x));
            x = kick.search(s2, x+1);
            assert(x == 3, text("real x is ", x));
            x = kick.search(s2, x+1);
            assert(x == 8, text("real x is ", x));
            auto rdot = regex(to!String(`...`));
            kick = Kick!Char(rdot, new uint[256]);
            assert(kick.length == 0);
            auto rN = regex(to!String(`a(b+|c+)x`));
            kick = Kick!Char(rN, new uint[256]);
            assert(kick.length == 3);
            assert(kick.search("ababx",0) == 2);
            assert(kick.search("abaacba",0) == 3);//expected inexact

        }
    }
    test_fixed!(ShiftOr)();
    test_flex!(ShiftOr)();
}

alias Kickstart = ShiftOr;


//State of VM thread
struct Thread(Mark)
{
    Thread* next;    //intrusive linked list
    uint pc;
    uint counter;    //loop counter
    uint uopCounter; //counts micro operations inside one macro instruction (e.g. BackRef)
    Group!Mark[1] matches;
}

//head-tail singly-linked list
struct ThreadList(Mark)
{
    alias T = Thread!Mark;
    T* tip = null, toe = null;
    //add new thread to the start of list
    void insertFront(T* t)
    {
        if(tip)
        {
            t.next = tip;
            tip = t;
        }
        else
        {
            t.next = null;
            tip = toe = t;
        }
    }
    //add new thread to the end of list
    void insertBack(T* t)
    {
        if(toe)
        {
            toe.next = t;
            toe = t;
        }
        else
            tip = toe = t;
        toe.next = null;
    }
    //move head element out of list
    T* fetch()
    {
        auto t = tip;
        if(tip == toe)
            tip = toe = null;
        else
            tip = tip.next;
        return t;
    }
    //non-destructive iteration of ThreadList
    struct ThreadRange
    {
        const(T)* ct;
        this(ThreadList tlist){ ct = tlist.tip; }
        @property bool empty(){ return ct is null; }
        @property const(T)* front(){ return ct; }
        @property popFront()
        {
            assert(ct);
            ct = ct.next;
        }
    }
    @property bool empty()
    {
        return tip == null;
    }
    ThreadRange opSlice()
    {
        return ThreadRange(this);
    }
}

//direction parameter for thompson one-shot match evaluator
enum OneShot { Fwd, Bwd };

import dpick.buffer;


/+
   Thomspon matcher does all matching in lockstep,
   never looking at the same char twice
+/
@trusted struct ThompsonMatcher(Char, Buffer)
    if(is(Char : dchar) && isBuffer!Buffer)
{
    alias Mark = Buffer.Mark;
    alias Re = Regex!Char;
    alias Thrd = Thread!Mark;
    alias ThrdList = ThreadList!Mark;
    alias G = Group!Mark;
    Thrd* freelist;
    ThrdList clist, nlist;
    size_t[] merge;
    G[] backrefed;
    Re re;           //regex program
    Buffer* buf;
    dchar front;
    size_t genCounter;    //merge trace counter, goes up on every dchar
    size_t threadSize;
    bool matched;
    bool exhausted;
    Mark index; //index before currently decoded char
    enum kicked = false;

    static size_t getThreadSize(const ref Re re)
    {
        return re.ngroup
            ? Thrd.sizeof + (re.ngroup-1)*G.sizeof
            : Thrd.sizeof - G.sizeof;
    }

    static size_t initialMemory(const ref Regex!Char re)
    {
        return getThreadSize(re)*re.threadCount + re.hotspotTableSize*size_t.sizeof;
    }

    //true if it's start of input
    @property bool atStart(){ return index == Mark.init; }

    //true if it's end of input
    @property bool atEnd(){  return front == dchar.init; }

    bool next()
    {
        if(buf.empty) {
            index = buf.mark(); //mark at end
            front = dchar.init;
            return false;
        }
        index = buf.mark();
        front = decodeUtf8(*buf);
        return true;
    }

    static if(kicked)
    {
        bool search()
        {

            if(!s.search(re.kickstart, front, index))
            {
                index = s.lastIndex;
                return false;
            }
            return true;
        }
    }

    void initExternalMemory(void[] memory)
    {
        threadSize = getThreadSize(re);
        prepareFreeList(re.threadCount, memory);
        if(re.hotspotTableSize)
        {
            merge = arrayInChunk!(size_t)(re.hotspotTableSize, memory);
            merge[] = 0;
        }
    }

    this()(Regex!Char program, Buffer* buffer, void[] memory)
    {
        re = program;
        buf = buffer;
        initExternalMemory(memory);
        genCounter = 0;
    }

    this(S)(ref ThompsonMatcher!(Char,S) matcher, Bytecode[] piece, Buffer* buffer)
    {
        buf = buffer;
        re = matcher.re;
        re.ir = piece;
        threadSize = matcher.threadSize;
        merge = matcher.merge;
        genCounter = matcher.genCounter;
        freelist = matcher.freelist;
        front = matcher.front;
        index = matcher.index;
    }

    auto fwdMatcher()(Bytecode[] piece)
    {
        auto m = ThompsonMatcher!(Char, Buffer)(this, piece, buf);
        return m;
    }
/*
    auto bwdMatcher()(Bytecode[] piece)
    {
        alias BackLooper = typeof(s.loopBack(index));
        auto m = ThompsonMatcher!(Char, BackLooper)(this, piece, s.loopBack(index));
        m.next();
        return m;
    }
*/
    auto dupTo(void[] memory)
    {
        typeof(this) tmp = this;//bitblit
        tmp.initExternalMemory(memory);
        tmp.genCounter = 0;
        return tmp;
    }

    enum MatchResult{
        NoMatch,
        PartialMatch,
        Match,
    }

    //match the input and fill matches
    bool match(G[] matches)
    {
        debug(std_regex_matcher)
            writeln("------------------------------------------");
        if(exhausted)
        {
            return false;
        }
        if(re.flags & RegexInfo.oneShot)
        {
            next();
            exhausted = true;
            return matchOneShot(matches)==MatchResult.Match;
        }
        static if(kicked)
            auto searchFn = re.kickstart.empty ? &this.next : &this.search;
        else
            auto searchFn = &this.next;
        
        if((!matched) && clist.empty)
        {
           searchFn();
           //pin here
        }
        else//char in question is  fetched in prev call to match
        {
            matched = false;
        }

        if(!atEnd)//if no char
        {
            for(;;)
            {
                genCounter++;
                debug(std_regex_matcher)
                {
                    writefln("Threaded matching threads at  %s", s[index..s.lastIndex]);
                    foreach(t; clist[])
                    {
                        assert(t);
                        writef("pc=%s ",t.pc);
                        write(t.matches);
                        writeln();
                    }
                }
                for(Thrd* t = clist.fetch(); t; t = clist.fetch())
                {
                    eval!true(t, matches);
                }
                if(!matched)//if we already have match no need to push the engine
                    eval!true(createStart(), matches);//new thread staring at this position
                else if(nlist.empty)
                {
                    debug(std_regex_matcher) writeln("Stopped  matching before consuming full input");
                    break;//not a partial match for sure
                }
                clist = nlist;
                nlist = (ThrdList).init;
                if(clist.tip is null)
                {
                    if(!searchFn())
                        break;
                    //pin here
                }
                else if(!next())
                {
                    if (!atEnd) return false;
                    exhausted = true;
                    break;
                }
            }
        }
        genCounter++; //increment also on each end
        debug(std_regex_matcher) writefln("Threaded matching threads at end");
        //try out all zero-width posibilities
        for(Thrd* t = clist.fetch(); t; t = clist.fetch())
        {
            eval!false(t, matches);
        }
        if(!matched)
            eval!false(createStart(), matches);//new thread starting at end of input
        if(matched)
        {//in case NFA found match along the way
         //and last possible longer alternative ultimately failed
            buf.restore(matches[0].end);//reset to last successful match
            next();//and reload front character
            //--- here the exact state of stream was restored ---
            exhausted = atEnd || !(re.flags & RegexOption.global);
            //+ empty match advances the input
            if(!exhausted && matches[0].begin == matches[0].end)
                next();
            //import std.stdio;
            //writeln((*buf)[matches[0].begin .. matches[0].end]);
        }
        return matched;
    }

    /+
        handle succesful threads
    +/
    void finish(Thrd* t, G[] matches)
    {
        matches.ptr[0..re.ngroup] = t.matches.ptr[0..re.ngroup];
        debug(std_regex_matcher)
        {
            writef("FOUND pc=%s prog_len=%s",
                    t.pc, re.ir.length);
            if(!matches.empty)
                writefln(": %s..%s", matches[0].begin, matches[0].end);
            foreach(v; matches)
                writefln("%d .. %d", v.begin, v.end);
        }
        matched = true;
    }

    /+
        match thread against codepoint, cutting trough all 0-width instructions
        and taking care of control flow, then add it to nlist
    +/
    void eval(bool withInput)(Thrd* t, G[] matches)
    {
        ThrdList worklist;
        debug(std_regex_matcher) writeln("---- Evaluating thread");
        for(;;)
        {
            debug(std_regex_matcher)
            {
                writef("\tpc=%s [", t.pc);
                foreach(x; worklist[])
                    writef(" %s ", x.pc);
                writeln("]");
            }
            switch(re.ir[t.pc].code)
            {
            case IR.End:
                finish(t, matches);
                matches[0].end = index; //fix endpoint of the whole match
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                debug(std_regex_matcher) writeln("Finished thread ", matches);
                return;
            case IR.Wordboundary:
                assert(0, "Not yet ready with buffer");
                /*dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    t.pc += IRL!(IR.Wordboundary);
                    break;
                }
                else if(atEnd && s.loopBack(index).nextChar(back, bi)
                        && wordTrie[back])
                {
                    t.pc += IRL!(IR.Wordboundary);
                    break;
                }
                else if(s.loopBack(index).nextChar(back, bi))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back];
                    if(af ^ ab)
                    {
                        t.pc += IRL!(IR.Wordboundary);
                        break;
                    }
                }
                recycle(t);
                t = worklist.fetch();
                if(!t)
                    return;
                break;*/
            case IR.Notwordboundary:
                assert(0, "Not yet ready with buffer");
                /*dchar back;
                DataIndex bi;
                //at start & end of input
                if(atStart && wordTrie[front])
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else if(atEnd && s.loopBack(index).nextChar(back, bi)
                        && wordTrie[back])
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else if(s.loopBack(index).nextChar(back, bi))
                {
                    bool af = wordTrie[front];
                    bool ab = wordTrie[back]  != 0;
                    if(af ^ ab)
                    {
                        recycle(t);
                        t = worklist.fetch();
                        if(!t)
                            return;
                        break;
                    }
                }
                t.pc += IRL!(IR.Wordboundary);
                break;*/
            case IR.Bol:
                assert(0, "Not yet ready with buffer");
                /*dchar back;
                DataIndex bi;
                if(atStart
                    ||( (re.flags & RegexOption.multiline)
                    && s.loopBack(index).nextChar(back,bi)
                    && startOfLine(back, front == '\n')))
                {
                    t.pc += IRL!(IR.Bol);
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;*/
            case IR.Eol:
                assert(0, "Not yet ready with buffer");
                /*debug(std_regex_matcher) writefln("EOL (front 0x%x) %s",  front, s[index..s.lastIndex]);
                dchar back;
                DataIndex bi;
                //no matching inside \r\n
                if(atEnd || ((re.flags & RegexOption.multiline)
                    && endOfLine(front, s.loopBack(index).nextChar(back, bi)
                        && back == '\r')))
                {
                    t.pc += IRL!(IR.Eol);
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;*/
            case IR.InfiniteStart, IR.InfiniteQStart:
                t.pc += re.ir[t.pc].data + IRL!(IR.InfiniteStart);
                goto case IR.InfiniteEnd; //both Q and non-Q
            case IR.RepeatStart, IR.RepeatQStart:
                t.pc += re.ir[t.pc].data + IRL!(IR.RepeatStart);
                goto case IR.RepeatEnd; //both Q and non-Q
            case IR.RepeatEnd:
            case IR.RepeatQEnd:
                //len, step, min, max
                uint len = re.ir[t.pc].data;
                uint step =  re.ir[t.pc+2].raw;
                uint min = re.ir[t.pc+3].raw;
                if(t.counter < min)
                {
                    t.counter += step;
                    t.pc -= len;
                    break;
                }
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                uint max = re.ir[t.pc+4].raw;
                if(t.counter < max)
                {
                    if(re.ir[t.pc].code == IR.RepeatEnd)
                    {
                        //queue out-of-loop thread
                        worklist.insertFront(fork(t, t.pc + IRL!(IR.RepeatEnd),  t.counter % step));
                        t.counter += step;
                        t.pc -= len;
                    }
                    else
                    {
                        //queue into-loop thread
                        worklist.insertFront(fork(t, t.pc - len,  t.counter + step));
                        t.counter %= step;
                        t.pc += IRL!(IR.RepeatEnd);
                    }
                }
                else
                {
                    t.counter %= step;
                    t.pc += IRL!(IR.RepeatEnd);
                }
                break;
            case IR.InfiniteEnd:
            case IR.InfiniteQEnd:
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                }
                else
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, index, genCounter, merge[re.ir[t.pc + 1].raw+t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                uint len = re.ir[t.pc].data;
                uint pc1, pc2; //branches to take in priority order
                if(re.ir[t.pc].code == IR.InfiniteEnd)
                {
                    pc1 = t.pc - len;
                    pc2 = t.pc + IRL!(IR.InfiniteEnd);
                }
                else
                {
                    pc1 = t.pc + IRL!(IR.InfiniteEnd);
                    pc2 = t.pc - len;
                }
                static if(withInput)
                {
                    int test = quickTestFwd(pc1, front, re);
                    if(test >= 0)
                    {
                        worklist.insertFront(fork(t, pc2, t.counter));
                        t.pc = pc1;
                    }
                    else
                        t.pc = pc2;
                }
                else
                {
                    worklist.insertFront(fork(t, pc2, t.counter));
                    t.pc = pc1;
                }
                break;
            case IR.OrEnd:
                if(merge[re.ir[t.pc + 1].raw+t.counter] < genCounter)
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) passed there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, s[index .. s.lastIndex], genCounter, merge[re.ir[t.pc + 1].raw + t.counter] );
                    merge[re.ir[t.pc + 1].raw+t.counter] = genCounter;
                    t.pc += IRL!(IR.OrEnd);
                }
                else
                {
                    debug(std_regex_matcher) writefln("A thread(pc=%s) got merged there : %s ; GenCounter=%s mergetab=%s",
                                    t.pc, s[index .. s.lastIndex], genCounter, merge[re.ir[t.pc + 1].raw + t.counter] );
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                }
                break;
            case IR.OrStart:
                t.pc += IRL!(IR.OrStart);
                goto case;
            case IR.Option:
                uint next = t.pc + re.ir[t.pc].data + IRL!(IR.Option);
                //queue next Option
                if(re.ir[next].code == IR.Option)
                {
                    worklist.insertFront(fork(t, next, t.counter));
                }
                t.pc += IRL!(IR.Option);
                break;
            case IR.GotoEndOr:
                t.pc = t.pc + re.ir[t.pc].data + IRL!(IR.GotoEndOr);
                goto case IR.OrEnd;
            case IR.GroupStart:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].begin = index;
                t.pc += IRL!(IR.GroupStart);
                break;
            case IR.GroupEnd:
                uint n = re.ir[t.pc].data;
                t.matches.ptr[n].end = index;
                t.pc += IRL!(IR.GroupEnd);
                break;
            case IR.Backref:
                uint n = re.ir[t.pc].data;
                G* source = re.ir[t.pc].localRef ? t.matches.ptr : backrefed.ptr;
                assert(source);
                if(source[n].begin == source[n].end)//zero-width Backref!
                {
                    t.pc += IRL!(IR.Backref);
                }
                else static if(withInput)
                {
                    //WTF with 
                    auto r = (*buf)[source[n].begin .. source[n].end];
                    auto s = cast(const(char)[])r[t.uopCounter .. $];
                    if(s.empty) //zero-width
                    {
                        t.pc += IRL!(IR.Backref);
                        t.uopCounter = 0;
                        break;
                    }
                    if(decodeFront(s) == front)
                    {
                        t.uopCounter += r.length - s.length;
                        if(s.empty)
                        {//last codepoint
                            t.pc += IRL!(IR.Backref);
                            t.uopCounter = 0;
                        }
                        nlist.insertBack(t);
                    }
                    else
                        recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                break;
            case IR.LookbehindStart:
            case IR.NeglookbehindStart:
                assert(0); //not specified yet with buffers
            case IR.LookaheadStart:
            case IR.NeglookaheadStart:
                auto save = index;
                uint len = re.ir[t.pc].data;
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                uint end = t.pc+len+IRL!(IR.LookaheadEnd)+IRL!(IR.LookaheadStart);
                bool positive = re.ir[t.pc].code == IR.LookaheadStart;
                /*static if(Stream.isLoopback)
                    auto matcher = bwdMatcher(re.ir[t.pc .. end]);
                else*/
                    auto matcher = fwdMatcher(re.ir[t.pc .. end]);
                matcher.re.ngroup = me - ms;
                matcher.backrefed = backrefed.empty ? t.matches : backrefed;
                bool nomatch = (matcher.matchOneShot(t.matches, IRL!(IR.LookaheadStart)) 
                    == MatchResult.Match) ^ positive;
                freelist = matcher.freelist;
                genCounter = matcher.genCounter;
                buf.restore(index);
                next();
                if(nomatch)
                {
                    recycle(t);
                    t = worklist.fetch();
                    if(!t)
                        return;
                    break;
                }
                else
                    t.pc = end;
                break;
            case IR.LookaheadEnd:
            case IR.NeglookaheadEnd:            
            case IR.LookbehindEnd:
            case IR.NeglookbehindEnd:
                t.pc = re.ir[t.pc].indexOfPair(t.pc);
                uint ms = re.ir[t.pc+1].raw, me = re.ir[t.pc+2].raw;
                finish(t, matches.ptr[ms..me]);
                recycle(t);
                //cut off low priority threads
                recycle(clist);
                recycle(worklist);
                return;
            case IR.Nop:
                t.pc += IRL!(IR.Nop);
                break;

                static if(withInput)
                {
            case IR.OrChar:
                      uint len = re.ir[t.pc].sequence;
                      uint end = t.pc + len;
                      static assert(IRL!(IR.OrChar) == 1);
                      for(; t.pc < end; t.pc++)
                          if(re.ir[t.pc].data == front)
                              break;
                      if(t.pc != end)
                      {
                          t.pc = end;
                          nlist.insertBack(t);
                      }
                      else
                          recycle(t);
                      t = worklist.fetch();
                      if(!t)
                          return;
                      break;
            case IR.Char:
                      if(front == re.ir[t.pc].data)
                      {
                          t.pc += IRL!(IR.Char);
                          nlist.insertBack(t);
                      }
                      else
                          recycle(t);
                      t = worklist.fetch();
                      if(!t)
                          return;
                      break;
            case IR.Any:
                      t.pc += IRL!(IR.Any);
                      if(!(re.flags & RegexOption.singleline)
                              && (front == '\r' || front == '\n'))
                          recycle(t);
                      else
                          nlist.insertBack(t);
                      t = worklist.fetch();
                      if(!t)
                          return;
                      break;
            case IR.CodepointSet:
                      if(re.charsets[re.ir[t.pc].data][front])
                      {
                          t.pc += IRL!(IR.CodepointSet);
                          nlist.insertBack(t);
                      }
                      else
                      {
                          recycle(t);
                      }
                      t = worklist.fetch();
                      if(!t)
                          return;
                      break;
            case IR.Trie:
                      if(re.tries[re.ir[t.pc].data][front])
                      {
                          t.pc += IRL!(IR.Trie);
                          nlist.insertBack(t);
                      }
                      else
                      {
                          recycle(t);
                      }
                      t = worklist.fetch();
                      if(!t)
                          return;
                      break;
                  default:
                      assert(0, "Unrecognized instruction " ~ re.ir[t.pc].mnemonic);
                }
                else
                {
                    default:
                        recycle(t);
                        t = worklist.fetch();
                        if(!t)
                            return;
                }
            }
        }

    }
    enum uint RestartPc = uint.max;
    //match the input, evaluating IR without searching
    MatchResult matchOneShot(G[] matches, uint startPc = 0)
    {
        debug(std_regex_matcher)
        {
            writefln("---------------single shot match ----------------- ");
        }
        alias eval evalFn;
        assert(clist == (ThrdList).init || startPc == RestartPc); // incorrect after a partial match
        assert(nlist == (ThrdList).init || startPc == RestartPc);
        if(!atEnd)//if no char
        {
            if(startPc!=RestartPc)
            {
                auto startT = createStart(startPc);
                genCounter++;
                evalFn!true(startT, matches);
            }
            for(;;)
            {
                debug(std_regex_matcher) writeln("\n-- Started iteration of main cycle");
                genCounter++;
                debug(std_regex_matcher)
                {
                    foreach(t; clist[])
                    {
                        assert(t);
                    }
                }
                for(Thrd* t = clist.fetch(); t; t = clist.fetch())
                {
                    evalFn!true(t, matches);
                }
                if(nlist.empty)
                {
                    debug(std_regex_matcher) writeln("Stopped  matching before consuming full input");
                    break;//not a partial match for sure
                }
                clist = nlist;
                nlist = (ThrdList).init;
                if(!next())
                {
                    if (!atEnd) return MatchResult.PartialMatch;
                    break;
                }
                debug(std_regex_matcher) writeln("-- Ended iteration of main cycle\n");
            }
        }
        genCounter++; //increment also on each end
        debug(std_regex_matcher) writefln("-- Matching threads at end");
        //try out all zero-width posibilities
        for(Thrd* t = clist.fetch(); t; t = clist.fetch())
        {
            evalFn!false(t, matches);
        }
        if(!matched)
            evalFn!false(createStart(startPc), matches);

        return (matched?MatchResult.Match:MatchResult.NoMatch);
    }

    //get a dirty recycled Thread
    Thrd* allocate()
    {
        assert(freelist, "not enough preallocated memory");
        Thrd* t = freelist;
        freelist = freelist.next;
        return t;
    }

    //link memory into a free list of Threads
    void prepareFreeList(size_t size, ref void[] memory)
    {
        void[] mem = memory[0 .. threadSize*size];
        memory = memory[threadSize * size .. $];
        freelist = cast(Thrd*)&mem[0];
        size_t i;
        for(i = threadSize; i < threadSize*size; i += threadSize)
            (cast(Thrd*)&mem[i-threadSize]).next = cast(Thrd*)&mem[i];
        (cast(Thrd*)&mem[i-threadSize]).next = null;
    }

    //dispose a thread
    void recycle(Thrd* t)
    {
        t.next = freelist;
        freelist = t;
    }

    //dispose list of threads
    void recycle(ref ThrdList list)
    {
        auto t = list.tip;
        while(t)
        {
            auto next = t.next;
            recycle(t);
            t = next;
        }
        list = list.init;
    }

    //creates a copy of master thread with given pc
    Thrd* fork(Thrd* master, uint pc, uint counter)
    {
        auto t = allocate();
        t.matches.ptr[0..re.ngroup] = master.matches.ptr[0..re.ngroup];
        t.pc = pc;
        t.counter = counter;
        t.uopCounter = 0;
        return t;
    }

    //creates a start thread
    Thrd* createStart(uint pc = 0)
    {
        auto t = allocate();
        t.matches.ptr[0..re.ngroup] = G.init;
        t.matches[0].begin = index;
        t.pc = pc;
        t.counter = 0;
        t.uopCounter = 0;
        return t;
    }
}

/++
    $(D Captures) object contains submatches captured during a call
    to $(D match) or iteration over $(D RegexMatch) range.

    First element of range is the whole match.

    Example, showing basic operations on $(D Captures):
    ----
    import std.regex;
    import std.range;

    void main()
    {
        auto m = match("@abc#", regex(`(\w)(\w)(\w)`));
        auto c = m.captures;
        assert(c.pre == "@"); // Part of input preceeding match
        assert(c.post == "#"); // Immediately after match
        assert(c.hit == c[0] && c.hit == "abc"); // The whole match
        assert(c[2] =="b");
        assert(c.front == "abc");
        c.popFront();
        assert(c.front == "a");
        assert(c.back == "c");
        c.popBack();
        assert(c.back == "b");
        popFrontN(c, 2);
        assert(c.empty);
    }
    ----
+/
@trusted public struct Captures(Char, Buffer)
    if(is(Char : dchar) && isBuffer!Buffer)
{//@trusted because of union inside
    alias Mark = Buffer.Mark;
    alias G = Group!Mark;
private:
    Buffer* _input;
    bool _empty;
    enum smallString = 3;
    union
    {
        G[] big_matches;
        G[smallString] small_matches;
    }
    uint _f, _b;
    uint _ngroup;
    NamedGroup[] _names;

    this()(Buffer* input, uint ngroups, NamedGroup[] named)
    {
        _input = input;
        _ngroup = ngroups;
        _names = named;
        newMatches();
        _b = _ngroup;
        _f = 0;
    }

    this(alias Engine)(ref RegexMatch!(Buffer, Engine) rmatch)
    {
        _input = rmatch._input;
        _ngroup = rmatch._engine.re.ngroup;
        _names = rmatch._engine.re.dict;
        newMatches();
        _b = _ngroup;
        _f = 0;
    }

    @property G[] matches()
    {
       return _ngroup > smallString ? big_matches : small_matches[0 .. _ngroup];
    }

    void newMatches()
    {
        if(_ngroup > smallString)
            big_matches = new G[_ngroup];
    }

    @property auto nullRange(){
        return typeof(_input.slice(matches[0].begin)).init;
    }

public:
    ///Slice of input prior to the match.
    /*@property auto pre()
    {
        return _empty ? nullRange: (*_input)[0 .. matches[0].begin];
    }*/

    ///Slice of input immediately after the match.
    /*@property auto post()
    {
        return _empty ? nullRange : (*_input)[matches[0].end .. $];
    }*/

    ///Slice of matched portion of input.
    @property auto hit()
    {
        assert(!_empty);
        return (*_input)[matches[0].begin .. matches[0].end];
    }

    ///Range interface.
    @property auto front()
    {
        assert(!empty);
        return (*_input)[matches[_f].begin .. matches[_f].end];
    }

    ///ditto
    @property auto back()
    {
        assert(!empty);
        return (*_input)[matches[_b - 1].begin .. matches[_b - 1].end];
    }

    ///ditto
    void popFront()
    {
        assert(!empty);
        ++_f;
    }

    ///ditto
    void popBack()
    {
        assert(!empty);
        --_b;
    }

    ///ditto
    @property bool empty() const { return _empty || _f >= _b; }

    ///ditto
    auto opIndex()(size_t i) /*const*/ //@@@BUG@@@
    {
        assert(_f + i < _b,text("requested submatch number ", i," is out of range"));
        //assert(matches[_f + i].begin <= matches[_f + i].end, 
        //    text("wrong match: ", matches[_f + i].begin, "..", matches[_f + i].end));
        return (*_input)[matches[_f + i].begin .. matches[_f + i].end];
    }

    /++
        Lookup named submatch.

        ---
        import std.regex;
        import std.range;

        auto m = match("a = 42;", regex(`(?P<var>\w+)\s*=\s*(?P<value>\d+);`));
        auto c = m.captures;
        assert(c["var"] == "a");
        assert(c["value"] == "42");
        popFrontN(c, 2);
        //named groups are unaffected by range primitives
        assert(c["var"] =="a");
        assert(c.front == "42");
        ----
    +/
    auto opIndex(String)(String i) /*const*/ //@@@BUG@@@
        if(isSomeString!String)
    {
        size_t index = lookupNamedGroup(_names, i);
        return (*_input)[matches[index].begin .. matches[index].end];
    }

    ///Number of matches in this object.
    @property size_t length() const { return _empty ? 0 : _b - _f;  }

    ///A hook for compatibility with original std.regex.
    @property ref captures(){ return this; }
}

/++
    A regex engine state, as returned by $(D match) family of functions.

    Effectively it's a forward range of Captures!R, produced
    by lazily searching for matches in a given input.

    $(D alias Engine) specifies an engine type to use during matching,
    and is automatically deduced in a call to $(D match)/$(D bmatch).
+/
@trusted public struct RegexMatch(R, alias Engine = ThompsonMatcher)
    if(isBuffer!R)
{
private:
    alias Engine!(char, R) EngineType;
    EngineType _engine;
    R* _input;
    Captures!(char, R) _captures;
    void[] _memory;//is ref-counted

    this(RegEx)(ref R input, RegEx prog)
    {
        _input = &input;
        immutable size = EngineType.initialMemory(prog)+size_t.sizeof;
        _memory = (enforce(malloc(size))[0..size]);
        scope(failure) free(_memory.ptr);
        *cast(size_t*)_memory.ptr = 1;
        _engine = EngineType(prog, _input, _memory[size_t.sizeof..$]);
        _captures = Captures!(char, R)(this);
        _captures._empty = !_engine.match(_captures.matches);
        debug(std_regex_allocation) writefln("RefCount (ctor): %x %d", _memory.ptr, counter);
    }

    @property ref size_t counter(){ return *cast(size_t*)_memory.ptr; }
public:
    this(this)
    {
        if(_memory.ptr)
        {
            ++counter;
            debug(std_regex_allocation) writefln("RefCount (postblit): %x %d",
                _memory.ptr, *cast(size_t*)_memory.ptr);
        }
    }

    ~this()
    {
        if(_memory.ptr && --*cast(size_t*)_memory.ptr == 0)
        {
            debug(std_regex_allocation) writefln("RefCount (dtor): %x %d",
                _memory.ptr, *cast(size_t*)_memory.ptr);
            free(cast(void*)_memory.ptr);
        }
    }

    ///Shorthands for front.pre, front.post, front.hit.
/*    @property R pre()
    {
        return _captures.pre;
    }

    ///ditto
    @property R post()
    {
        return _captures.post;
    }
*/
    ///ditto
    @property auto hit()
    {
        return _captures.hit;
    }

    /++
        Functionality for processing subsequent matches of global regexes via range interface:
        ---
        import std.regex;
        auto m = match("Hello, world!", regex(`\w+`, "g"));
        assert(m.front.hit == "Hello");
        m.popFront();
        assert(m.front.hit == "world");
        m.popFront();
        assert(m.empty);
        ---
    +/
    @property auto ref front()
    {
        return _captures;
    }

    ///ditto
    void popFront()
    {

        if(counter != 1)
        {//do cow magic first
            counter--;//we abandon this reference
            immutable size = EngineType.initialMemory(_engine.re)+size_t.sizeof;
            _memory = (enforce(malloc(size))[0..size]);
            _engine = _engine.dupTo(_memory[size_t.sizeof..size]);
            counter = 1;//points to new chunk
        }
        //previous _captures can have escaped references from Capture object
        _captures.newMatches();
        _captures._empty = !_engine.match(_captures.matches);
    }

    ///ditto
    auto save(){ return this; }

    ///Test if this match object is empty.
    @property bool empty(){ return _captures._empty; }

    ///Same as !(x.empty), provided for its convenience  in conditional statements.
    T opCast(T:bool)(){ return !empty; }

    /// Same as .front, provided for compatibility with original std.regex.
    @property auto captures(){ return _captures; }

}

private @trusted auto matchOnce(alias Engine, RegEx, Buffer)
    (ref Buffer input, RegEx re) if(isBuffer!Buffer)
{
    alias Char = char;
    alias EngineType = Engine!(Char, Buffer);
    size_t size = EngineType.initialMemory(re);
    void[] memory = enforce(malloc(size))[0..size];
    scope(exit) free(memory.ptr);
    auto captures = Captures!(Char, Buffer)(&input, re.ngroup, re.dict);
    auto engine = EngineType(re, &input, memory);        
    captures._empty = !engine.match(captures.matches);
    return captures;
}

private auto matchMany(alias Engine, RegEx, R)(ref R input, RegEx re)
{
    re.flags |= RegexOption.global;
    return RegexMatch!(R, Engine)(input, re);
}

unittest
{
    //sanity checks for new API
    auto re = regex("abc");
    auto b = buffer("abc");
    auto b2 = buffer("abc");
    assert(!b.matchOnce!(ThompsonMatcher)(re).empty);
    auto m = b2.matchOnce!(ThompsonMatcher)(re);
    assert(m[0] == "abc", text(m[0]));
}
/++
    Compile regular expression pattern for the later execution.
    Returns: $(D Regex) object that works on inputs having
    the same character width as $(D pattern).

    Params:
    pattern = Regular expression
    flags = The _attributes (g, i, m and x accepted)

    Throws: $(D RegexException) if there were any errors during compilation.
+/
@trusted public auto regex(S)(S pattern, const(char)[] flags="")
    if(isSomeString!(S))
{
    enum cacheSize = 8; //TODO: invent nice interface to control regex caching
    if(__ctfe)
        return regexImpl(pattern, flags);
    return memoize!(regexImpl!S, cacheSize)(pattern, flags);
}

public auto regexImpl(S)(S pattern, const(char)[] flags="")
    if(isSomeString!(S))
{
    auto parser = Parser!(Unqual!(typeof(pattern)))(pattern, flags);
    auto r = parser.program;
    return r;
}

template isRegexFor(RegEx, R)
{
    enum isRegexFor = is(RegEx == Regex!(BasicElementOf!R));
}


//UTF-8 buffer
public auto matchFirst(R, RegEx)(ref R input, RegEx re)
    if(isBuffer!R && is(RegEx == Regex!(char)))
{
    return matchOnce!ThompsonMatcher(input, re);
}

///ditto
public auto matchFirst(R, String)(ref R input, String re)
    if(isBuffer!R && isSomeString!String)
{
    return matchOnce!ThompsonMatcher(input, regex(re));
}


public auto matchAll(R, RegEx)(ref R input, RegEx re)
    if(isBuffer!R && is(RegEx == Regex!(char)))
{
    return matchMany!ThompsonMatcher(input, re);
}

///ditto
public auto matchAll(R, String)(ref R input, String re)
    if(isBuffer!R && isSomeString!String)
{
    return matchMany!ThompsonMatcher(input, regex(re));
}

// another set of tests just to cover the new API
@system unittest
{
    foreach(String; TypeTuple!(string))
    {
        auto str1 = "blah-bleh".to!String().buffer;
        auto pat1 = "bl[ae]h".to!String();
        auto mf = matchFirst(str1, pat1);
        assert(mf.equal(["blah".to!String()]));
        str1 = "blah-bleh".to!String().buffer;
        auto mAll = matchAll(str1, pat1);        
        
        assert(mAll.equal!((a,b) => a.equal(b))
            ([["blah".to!String()], ["bleh".to!String()]]));

        auto str2 = "1/03/12 - 3/03/12".to!String().buffer;
        auto pat2 = regex(r"(\d+)/(\d+)/(\d+)".to!String());
        auto mf2 = matchFirst(str2, pat2);
        assert(mf2.equal(["1/03/12", "1", "03", "12"].map!(to!String)()));
        str2 = "1/03/12 - 3/03/12".to!String().buffer;
        auto mAll2 = matchAll(str2, pat2);
        assert(mAll2.front.equal(mf2));
        mAll2.popFront();
        assert(mAll2.front.equal(["3/03/12", "3", "03", "12"].map!(to!String)()));
        mf2.popFrontN(3);
        assert(mf2.equal(["12".to!String()]));
    }
}

//produce replacement string from format using captures for substitution
private void replaceFmt(R, Capt, OutR)
    (R format, Capt captures, OutR sink, bool ignoreBadSubs = false)
    if(isOutputRange!(OutR, ElementEncodingType!R) &&
        isOutputRange!(OutR, ElementEncodingType!(typeof(captures[0]))))
{
    enum State { Normal, Dollar }
    auto state = State.Normal;
    size_t offset;
L_Replace_Loop:
    while(!format.empty)
        final switch(state)
        {
        case State.Normal:
            for(offset = 0; offset < format.length; offset++)//no decoding
            {
                if(format[offset] == '$')
                {
                    state = State.Dollar;
                    sink.put(format[0 .. offset]);
                    format = format[offset+1 .. $];//ditto
                    continue L_Replace_Loop;
                }
            }
            sink.put(format[0 .. offset]);
            format = format[offset .. $];
            break;
        case State.Dollar:
            if(ascii.isDigit(format[0]))
            {
                uint digit = parse!uint(format);
                enforce(ignoreBadSubs || digit < captures.length, text("invalid submatch number ", digit));
                if(digit < captures.length)
                    sink.put(cast(char[])captures[digit]);
            }
            else if(format[0] == '{')
            {
                auto x = find!(a => !ascii.isAlpha(a))(format[1..$]);
                enforce(!x.empty && x[0] == '}', "no matching '}' in replacement format");
                auto name = format[1 .. $ - x.length];
                format = x[1..$];
                enforce(!name.empty, "invalid name in ${...} replacement format");
                sink.put(cast(char[])captures[name]);
            }
            else if(format[0] == '&')
            {
                //Hack around - @@@BUG@@@@ putting ubyte[] gives bogus results
                sink.put(cast(char[])captures[0]);
                format = format[1 .. $];
            }
            //Not available for buffered regex
            /*else if(format[0] == '`')
            {
                sink.put(captures.pre);
                format = format[1 .. $];
            }
            else if(format[0] == '\'')
            {
                sink.put(captures.post);
                format = format[1 .. $];
            }*/
            else if(format[0] == '$')
            {
                sink.put(format[0 .. 1]);
                format = format[1 .. $];
            }
            state = State.Normal;
            break;
        }
    enforce(state == State.Normal, "invalid format string in regex replace");
}

///Exception object thrown in case of errors during regex compilation.
public class RegexException : Exception
{
    ///
    @trusted this(string msg, string file = __FILE__, size_t line = __LINE__)
    {//@@@BUG@@@ Exception constructor is not @safe
        super(msg, file, line);
    }
}

//--------------------- TEST SUITE ---------------------------------
version(unittest)
{
@system:

/* The test vectors in this file are altered from Henry Spencer's regexp
   test code. His copyright notice is:

        Copyright (c) 1986 by University of Toronto.
        Written by Henry Spencer.  Not derived from licensed software.

        Permission is granted to anyone to use this software for any
        purpose on any computer system, and to redistribute it freely,
        subject to the following restrictions:

        1. The author is not responsible for the consequences of use of
                this software, no matter how awful, even if they arise
                from defects in it.

        2. The origin of this software must not be misrepresented, either
                by explicit claim or by omission.

        3. Altered versions must be plainly marked as such, and must not
                be misrepresented as being the original software.


 */

unittest
{
    struct TestVectors
    {
        string pattern;
        string input;
        string result;
        string format;
        string replace;
        string flags;
    }

    enum TestVectors tv[] = [
//        TestVectors(  "a\\b",       "a",  "y",    "$&",    "a" ),
//        TestVectors(  "(a)b\\1",   "abaab","y",    "$&",    "aba" ),
//        TestVectors(  "()b\\1",     "aaab", "y",    "$&",    "b" ),
        TestVectors(  "abc",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "abc",       "xbc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "axc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "abx",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "xabcy","y",    "$&",    "abc" ),
        TestVectors(  "abc",       "ababc","y",    "$&",    "abc" ),
        TestVectors(  "ab*c",      "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab*bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab+bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab+bc",     "abc",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abq",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab?bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab?bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab?bc",     "abbbbc","n",   "-",    "-" ),
        TestVectors(  "ab?c",      "abc",  "y",    "$&",    "abc" ),
/*        TestVectors(  "^abc$",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "abcc", "n",    "-",    "-" ),
        TestVectors(  "^abc",      "abcc", "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "aabc", "n",    "-",    "-" ),
        TestVectors(  "abc$",      "aabc", "y",    "$&",    "abc" ),
        TestVectors(  "^",         "abc",  "y",    "$&",    "" ),
        TestVectors(  "$",         "abc",  "y",    "$&",    "" ),
*/        TestVectors(  "a.c",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a.c",       "axc",  "y",    "$&",    "axc" ),
        TestVectors(  "a.*c",      "axyzc","y",    "$&",    "axyzc" ),
        TestVectors(  "a.*c",      "axyzd","n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abc",  "n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abd",  "y",    "$&",    "abd" ),
        TestVectors(  "a[b-d]e",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[b-d]e",   "ace",  "y",    "$&",    "ace" ),
        TestVectors(  "a[b-d]",    "aac",  "y",    "$&",    "ac" ),
        TestVectors(  "a[-b]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-a]",    "-",    "c",    "-",    "-" ),
        TestVectors(  "a[]b",      "-",    "c",    "-",    "-" ),
        TestVectors(  "a[",        "-",    "c",    "-",    "-" ),
        TestVectors(  "a]",        "a]",   "y",    "$&",    "a]" ),
        TestVectors(  "a[\\]]b",     "a]b",  "y",  "$&",    "a]b" ),
        TestVectors(  "a[^bc]d",   "aed",  "y",    "$&",    "aed" ),
        TestVectors(  "a[^bc]d",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[^-b]c",   "adc",  "y",    "$&",    "adc" ),
        TestVectors(  "a[^-b]c",   "a-c",  "n",    "-",    "-" ),
        TestVectors(  "a[^\\]b]c",   "adc",  "y",  "$&",    "adc" ),
        TestVectors(  "ab|cd",     "abc",  "y",    "$&",    "ab" ),
        TestVectors(  "ab|cd",     "abcd", "y",    "$&",    "ab" ),
        TestVectors(  "()ef",      "def",  "y",    "$&-$1",        "ef-" ),
        TestVectors(  "()*",       "-",    "y",    "-",    "-" ),
        TestVectors(  "*a",        "-",    "c",    "-",    "-" ),
//        TestVectors(  "^*",        "-",    "y",    "-",    "-" ),
//        TestVectors(  "$*",        "-",    "y",    "-",    "-" ),
        TestVectors(  "(*)b",      "-",    "c",    "-",    "-" ),
//        TestVectors(  "$b",        "b",    "n",    "-",    "-" ),
        TestVectors(  "a\\",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a\\(b",     "a(b",  "y",    "$&-$1",        "a(b-" ),
        TestVectors(  "a\\(*b",    "ab",   "y",    "$&",    "ab" ),
        TestVectors(  "a\\(*b",    "a((b", "y",    "$&",    "a((b" ),
        TestVectors(  "a\\\\b",    "a\\b", "y",    "$&",    "a\\b" ),
        TestVectors(  "abc)",      "-",    "c",    "-",    "-" ),
        TestVectors(  "(abc",      "-",    "c",    "-",    "-" ),
        TestVectors(  "((a))",     "abc",  "y",    "$&-$1-$2",    "a-a-a" ),
        TestVectors(  "(a)b(c)",   "abc",  "y",    "$&-$1-$2",    "abc-a-c" ),
        TestVectors(  "a+b+c",     "aabbabc","y",  "$&",    "abc" ),
        TestVectors(  "a**",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a*?a",      "aa",   "y",    "$&",    "a" ),
        TestVectors(  "(a*)*",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a*)+",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a|)*",     "-",    "y",    "-",    "-" ),
        TestVectors(  "(a*|b)*",   "aabb", "y",    "-",    "-" ),
        TestVectors(  "(a|b)*",    "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)*",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)+",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)?",   "ab",   "y",    "$&-$1",        "a-a" ),
        TestVectors(  "[^ab]*",    "cde",  "y",    "$&",    "cde" ),
//        TestVectors(  "(^)*",      "-",    "y",    "-",    "-" ),
        TestVectors(  "(ab|)*",    "-",    "y",    "-",    "-" ),
        TestVectors(  ")(",        "-",    "c",    "-",    "-" ),
        TestVectors(  "",  "abc",  "y",    "$&",    "" ),
        TestVectors(  "abc",       "",     "n",    "-",    "-" ),
        TestVectors(  "a*",        "",     "y",    "$&",    "" ),
        TestVectors(  "([abc])*d", "abbbcd",       "y",    "$&-$1",        "abbbcd-c" ),
        TestVectors(  "([abc])*bcd", "abcd",       "y",    "$&-$1",        "abcd-a" ),
        TestVectors(  "a|b|c|d|e", "e",    "y",    "$&",    "e" ),
        TestVectors(  "(a|b|c|d|e)f", "ef",        "y",    "$&-$1",        "ef-e" ),
        TestVectors(  "((a*|b))*", "aabb", "y",    "-",    "-" ),
        TestVectors(  "abcd*efg",  "abcdefg",      "y",    "$&",    "abcdefg" ),
        TestVectors(  "ab*",       "xabyabbbz",    "y",    "$&",    "ab" ),
        TestVectors(  "ab*",       "xayabbbz",     "y",    "$&",    "a" ),
        TestVectors(  "(ab|cd)e",  "abcde",        "y",    "$&-$1",        "cde-cd" ),
        TestVectors(  "[abhgefdc]ij",      "hij",  "y",    "$&",    "hij" ),
//        TestVectors(  "^(ab|cd)e", "abcde",        "n",    "x$1y",        "xy" ),
        TestVectors(  "(abc|)ef",  "abcdef",       "y",    "$&-$1",        "ef-" ),
        TestVectors(  "(a|b)c*d",  "abcd",         "y",    "$&-$1",        "bcd-b" ),
        TestVectors(  "(ab|ab*)bc",        "abc",  "y",    "$&-$1",        "abc-a" ),
        TestVectors(  "a([bc]*)c*",        "abc",  "y",    "$&-$1",        "abc-bc" ),
        TestVectors(  "a([bc]*)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]+)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]*)(c+d)",     "abcd", "y",    "$&-$1-$2",    "abcd-b-cd" ),
        TestVectors(  "a[bcd]*dcdcde",     "adcdcde",      "y",    "$&",    "adcdcde" ),
        TestVectors(  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" ),
        TestVectors(  "(ab|a)b*c", "abc",           "y",    "$&-$1",        "abc-ab" ),
        TestVectors(  "((a)(b)c)(d)",      "abcd",  "y",    "$1-$2-$3-$4",      "abc-a-b-d" ),
        TestVectors(  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "$&",    "alpha" ),
//        TestVectors(  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "$&-$1",        "bh-" ),
/*        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effgz",        "y",    "$&-$1-$2",    "effgz-effgz-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "ij",   "y",    "$&-$1-$2",    "ij-ij-j" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effg", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "bcdd", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "reffgz",       "y",    "$&-$1-$2",    "effgz-effgz-" ),
*/        TestVectors(  "(((((((((a)))))))))",       "a",    "y",    "$&",    "a" ),
        TestVectors(  "multiple words of text",    "uh-uh",        "n",    "-",    "-" ),
        TestVectors(  "multiple words",    "multiple words, yeah", "y",    "$&",    "multiple words" ),
        TestVectors(  "(.*)c(.*)", "abcde",                "y",    "$&-$1-$2",    "abcde-ab-de" ),
        TestVectors(  "\\((.*), (.*)\\)",  "(a, b)",       "y",    "($2, $1)",   "(b, a)" ),
        TestVectors(  "abcd",      "abcd",                   "y",    "$&-&-$$$&",  "abcd-&-$abcd" ),
        TestVectors(  "a(bc)d",    "abcd",                 "y",    "$1-$$1-$$$1",    "bc-$1-$bc" ),
        TestVectors(  "[k]",                       "ab",   "n",    "-",    "-" ),
        TestVectors(  "[ -~]*",                    "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~]*",                 "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~]*",              "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~]*",           "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a{2}",      "candy",                "n",    "",     "" ),
        TestVectors(  "a{2}",      "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2}",      "caaandy",              "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "candy",                "n",    "",     "" ),
        TestVectors(  "a{2,}",     "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "caaaaaandy",           "y",    "$&",    "aaaaaa" ),
        TestVectors(  "a{1,3}",    "cndy",                 "n",    "",     "" ),
        TestVectors(  "a{1,3}",    "candy",                "y",    "$&",    "a" ),
        TestVectors(  "a{1,3}",    "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{1,3}",    "caaaaaandy",           "y",    "$&",    "aaa" ),
        TestVectors(  "e?le?",     "angel",                "y",    "$&",    "el" ),
        TestVectors(  "e?le?",     "angle",                "y",    "$&",    "le" ),
/*        TestVectors(  "\\bn\\w",   "noonday",              "y",    "$&",    "no" ),
        TestVectors(  "\\wy\\b",   "possibly yesterday",   "y",    "$&",    "ly" ),
        TestVectors(  "\\w\\Bn",   "noonday",              "y",    "$&",    "on" ),
        TestVectors(  "y\\B\\w",   "possibly yesterday",   "y",    "$&",    "ye" ),
*/        TestVectors(  "\\cJ",      "abc\ndef",             "y",    "$&",    "\n" ),
        TestVectors(  "\\d",       "B2 is",                "y",    "$&",    "2" ),
        TestVectors(  "\\D",       "B2 is",                "y",    "$&",    "B" ),
        TestVectors(  "\\s\\w*",   "foo bar",              "y",    "$&",    " bar" ),
        TestVectors(  "\\S\\w*",   "foo bar",              "y",    "$&",    "foo" ),
        TestVectors(  "abc",       "ababc",                "y",    "$&",    "abc" ),
        TestVectors(  "apple(,)\\sorange\\1",      "apple, orange, cherry, peach", "y", "$&", "apple, orange," ),
        TestVectors(  "(\\w+)\\s(\\w+)",           "John Smith", "y", "$2, $1", "Smith, John" ),
        TestVectors(  "\\n\\f\\r\\t\\v",           "abc\n\f\r\t\vdef", "y", "$&", "\n\f\r\t\v" ),
        TestVectors(  ".*c",       "abcde",                        "y",    "$&",    "abc" ),
//        TestVectors(  "^\\w+((;|=)\\w+)+$", "some=host=tld",    "y", "$&-$1-$2", "some=host=tld-=tld-=" ),
//        TestVectors(  "^\\w+((\\.|-)\\w+)+$", "some.host.tld",    "y", "$&-$1-$2", "some.host.tld-.tld-." ),
        TestVectors(  "q(a|b)*q",  "xxqababqyy",                "y",    "$&-$1",        "qababq-b" ),
/*        TestVectors(  "^(a)(b){0,1}(c*)",   "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b)?(c*)",       "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b){0,1}(c*)",   "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "acc",  "y", "$1 $2 $3", "a  " ),
        TestVectors(  "^(a)(b)?(c*)",       "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "acc",  "y", "$1 $2 $3", "a  " ),
*/        TestVectors(  "(?:ab){3}",       "_abababc","y", "$&-$1",    "ababab-" ),
        TestVectors(  "(?:a(?:x)?)+",    "aaxaxx",  "y", "$&-$1-$2", "aaxax--" ),
        TestVectors(  `\W\w\W`,         "aa b!ca",  "y", "$&",       " b!"),
//more repetitions:
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}",  "aaabaaaabbb", "y", "$&", "aaabaaaabbb" ),
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}?", "aaabaaaabbb", "y", "$&", "aaab" ),
//groups:
        TestVectors(  "(abc)|(edf)|(xyz)",     "xyz",             "y",   "$1-$2-$3","--xyz"),
        TestVectors(  "(?P<q>\\d+)/(?P<d>\\d+)",     "2/3",       "y",     "${d}/${q}",    "3/2"),
//set operations:
        TestVectors(  "[a-z--d-f]",                  " dfa",      "y",   "$&",     "a"),
        TestVectors(  "[abc[pq--acq]]{2}",           "bqpaca",    "y",   "$&",     "pa"),
        TestVectors(  "[a-z9&&abc0-9]{3}",           "z90a0abc",  "y",   "$&",     "abc"),
        TestVectors(  "[0-9a-f~~0-5a-z]{2}",         "g0a58x",    "y",   "$&",     "8x"),
        TestVectors(  "[abc[pq]xyz[rs]]{4}",         "cqxr",      "y",   "$&",     "cqxr"),
        TestVectors(  "[abcdf--[ab&&[bcd]][acd]]",   "abcdefgh",  "y",   "$&",     "f"),
//unicode blocks & properties:
        TestVectors(  `\P{Inlatin1suppl ement}`, "\u00c2!", "y", "$&", "!"),
        TestVectors(  `\p{InLatin-1 Supplement}\p{in-mathematical-operators}\P{Inlatin1suppl ement}`, "\u00c2\u2200\u00c3\u2203.", "y", "$&", "\u00c3\u2203."),
        TestVectors(  `[-+*/\p{in-mathematical-operators}]{2}`,    "a+\u2212",    "y",    "$&",    "+\u2212"),
        TestVectors(  `\p{Ll}+`,                      "XabcD",    "y",  "$&",      "abc"),
        TestVectors(  `\p{Lu}+`,                      "абвГДЕ",   "y",  "$&",      "ГДЕ"),
//        TestVectors(  `^\p{Currency Symbol}\p{Sc}`    "$₤",       "y",  "$&",      "$₤"),
        TestVectors(  `\p{Common}\p{Thai}`            "!ฆ",       "y",  "$&",      "!ฆ"),
        TestVectors(  `[\d\s]*\D`,  "12 \t3\U00001680\u0F20_2",   "y",  "$&", "12 \t3\U00001680\u0F20_"),
        TestVectors(  `[c-wф]фф`, "ффф", "y", "$&", "ффф"),
//case insensitive:
//        TestVectors(   `^abcdEf$`,           "AbCdEF"               "y",   "$&", "AbCdEF",      "i"),
        TestVectors(   `Русский язык`,    "рУсскИй ЯзЫк",           "y",   "$&", "рУсскИй ЯзЫк",     "i"),
        TestVectors(   `ⒶⒷⓒ` ,        "ⓐⓑⒸ",                   "y",   "$&", "ⓐⓑⒸ",      "i"),
        TestVectors(   "\U00010400{2}",  "\U00010428\U00010400 ",   "y",   "$&", "\U00010428\U00010400", "i"),
        TestVectors(   `[adzУ-Я]{4}`,    "DzюА"                     "y",   "$&", "DzЮа", "i"),
        TestVectors(   `\p{L}\p{Lu}{10}`, "абвгдеЖЗИКЛ",            "y",   "$&", "абвгдеЖЗИКЛ", "i"),
        TestVectors(   `(?:Dåb){3}`,  "DåbDÅBdÅb",                  "y",   "$&", "DåbDÅBdÅb", "i"),
//escapes:
        TestVectors(    `\u0041\u005a\U00000065\u0001`,         "AZe\u0001",       "y",   "$&", "AZe\u0001"),
        TestVectors(    `\u`,               "",   "c",   "-",  "-"),
        TestVectors(    `\U`,               "",   "c",   "-",  "-"),
        TestVectors(    `\u003`,            "",   "c",   "-",  "-"),
        TestVectors(    `[\x00-\x7f]{4}`,        "\x00\x09ab",   "y", "$&", "\x00\x09ab"),
        TestVectors(    `[\cJ\cK\cA-\cD]{3}\cQ`, "\x01\x0B\x0A\x11", "y", "$&", "\x01\x0B\x0A\x11"),
        TestVectors(    `\r\n\v\t\f\\`,     "\r\n\v\t\f\\",   "y",   "$&", "\r\n\v\t\f\\"),
        TestVectors(    `[\u0003\u0001]{2}`,  "\u0001\u0003",         "y",   "$&", "\u0001\u0003"),
//        TestVectors(    `^[\u0020-\u0080\u0001\n-\r]{8}`,  "abc\u0001\v\f\r\n",  "y",   "$&", "abc\u0001\v\f\r\n"),
        TestVectors(    `\w+\S\w+`, "ab7!44c",  "y", "$&", "ab7!44c"),
/*        TestVectors(    `\b\w+\b`,  " abde4 ",  "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  " abde4",   "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  "abde4 ",   "y", "$&", "abde4"),
        TestVectors(    `\pL\pS`,   "a\u02DA",  "y", "$&", "a\u02DA"),
        TestVectors(    `\pX`,      "",         "c", "-",  "-"),
// ^, $, \b, \B, multiline :
        TestVectors(    `\r.*?$`,    "abc\r\nxy", "y", "$&", "\r\nxy", "sm"),
        TestVectors(    `^a$^b$`,    "a\r\nb\n",  "n", "$&", "-", "m"),
        TestVectors(    `^a$\r\n^b$`,"a\r\nb\n",  "y", "$&", "a\r\nb", "m"),
        TestVectors(    `^$`,        "\r\n",      "y", "$&", "", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u2028","y", "$&", "a\nx", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u2029","y", "$&", "a\nx", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u0085","y", "$&", "a\nx","m"),
        TestVectors(    `^x$`,       "\u2028x",   "y", "$&", "x", "m"),
        TestVectors(    `^x$`,       "\u2029x",   "y", "$&", "x", "m"),
        TestVectors(    `^x$`,       "\u0085x",   "y", "$&", "x", "m"),
        TestVectors(    `\b^.`,      "ab",        "y", "$&", "a"),
        TestVectors(    `\B^.`,      "ab",        "n", "-",  "-"),
        TestVectors(    `^ab\Bc\B`,  "\r\nabcd",  "y", "$&", "abc", "m"),
        TestVectors(    `^.*$`,      "12345678",  "y", "$&", "12345678"),

// luckily obtained regression on incremental matching in backtracker
        TestVectors(  `^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([^ ]*)\s*#|# (?:\w|_)+=((?:\w|_)+))`,
            "0020  ; White_Space # ", "y", "$1-$2-$3", "--0020"),
*/
//lookahead
        TestVectors(    "(foo.)(?=(bar))",     "foobar foodbar", "y", "$&-$1-$2", "food-food-bar" ),
//        TestVectors(    `\b(\d+)[a-z](?=\1)`,  "123a123",        "y", "$&-$1", "123a-123" ),
//        TestVectors(    `\$(?!\d{3})\w+`,      "$123 $abc",      "y", "$&", "$abc"),
        TestVectors(    `(abc)(?=(ed(f))\3)`,    "abcedff",      "y", "-", "-"),
//        TestVectors(    `\b[A-Za-z0-9.]+(?=(@(?!gmail)))`, "a@gmail,x@com",  "y", "$&-$1", "x-@"),
        TestVectors(    `x()(abc)(?=(d)(e)(f)\2)`,   "xabcdefabc", "y", "$&", "xabc"),
        TestVectors(    `x()(abc)(?=(d)(e)(f)()\3\4\5)`,   "xabcdefdef", "y", "$&", "xabc"),
//lookback
/*        TestVectors(    `(?<=(ab))\d`,    "12ba3ab4",    "y",   "$&-$1", "4-ab",  "i"),
        TestVectors(    `\w(?<!\d)\w`,   "123ab24",  "y",   "$&", "ab"),
        TestVectors(    `(?<=Dåb)x\w`,  "DåbDÅBxdÅb",  "y",   "$&", "xd", "i"),
        TestVectors(    `(?<=(ab*c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(    `(?<=(ab*?c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(    `(?<=(a.*?c))x`,   "ababbcxac",  "y",   "$&-$1", "x-abbc"),
        TestVectors(    `(?<=(a{2,4}b{1,3}))x`,   "yyaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(    `(?<=((?:a{2,4}b{1,3}){1,2}))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aabbbaaaab"),
        TestVectors(    `(?<=((?:a{2,4}b{1,3}){1,2}?))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(    `(?<=(abc|def|aef))x`,    "abcx", "y",        "$&-$1",  "x-abc"),
        TestVectors(    `(?<=(abc|def|aef))x`,    "aefx", "y",        "$&-$1",  "x-aef"),
        TestVectors(    `(?<=(abc|dabc))(x)`,    "dabcx", "y",        "$&-$1-$2",  "x-abc-x"),
        TestVectors(    `(?<=(|abc))x`,        "dabcx", "y",        "$&-$1",  "x-"),
        TestVectors(    `(?<=((ab|da)*))x`,    "abdaabx", "y",        "$&-$2-$1",  "x-ab-abdaab"),
        TestVectors(    `a(?<=(ba(?<=(aba)(?<=aaba))))`, "aabaa", "y", "$&-$1-$2", "a-ba-aba"),
        TestVectors(    `.(?<!b).`,   "bax",  "y", "$&", "ax"),
        TestVectors(    `(?<=b(?<!ab)).`,   "abbx",  "y",  "$&", "x"),
        TestVectors(    `(?<=\.|[!?]+)X`,   "Hey?!X", "y", "$&", "X"),
        TestVectors(    `(?<=\.|[!?]+)a{3}`,   ".Nope.aaaX", "y", "$&", "aaa"),
//mixed lookaround
        TestVectors(   `a(?<=a(?=b))b`,    "ab", "y",      "$&", "ab"),
        TestVectors(   `a(?<=a(?!b))c`,    "ac", "y",      "$&", "ac"),
*/    ];
    string produceExpected(M,String)(auto ref M m, String fmt)
    {
        auto app = appender!(String)();
        replaceFmt(fmt, m.captures, app, true);
        return app.data;
    }
    void run_tests(alias matchFn)()
    {
        int i;
        foreach(Char; TypeTuple!(char))
        {
            alias immutable(Char)[] String;
            String produceExpected(M,Range)(auto ref M m, Range fmt)
            {
                auto app = appender!(String)();
                replaceFmt(fmt, m.captures, app, true);
                return app.data;
            }
            Regex!(Char) r;
            foreach(a, tvd; tv)
            {
                uint c = tvd.result[0];
                debug(std_regex_test) writeln(" Test #", a, " pattern: ", tvd.pattern, " with Char = ", Char.stringof);
                try
                {
                    i = 1;
                    r = regex(to!(String)(tvd.pattern), tvd.flags);
                }
                catch (RegexException e)
                {
                    i = 0;
                    debug(std_regex_test) writeln(e.msg);
                }

                assert((c == 'c') ? !i : i, "failed to compile pattern "~tvd.pattern);

                if(c != 'c')
                {
                    auto buf = buffer(tvd.input.to!String);                    
                    auto m = matchFn(buf, r);
                    i = !m.empty;
                    assert((c == 'y') ? i : !i, 
                        text(matchFn.stringof ~": failed to match pattern #", a ,": ", tvd.pattern));
                    if(c == 'y')
                    {
                        auto result = produceExpected(m, to!(String)(tvd.format));
                        assert(result == to!String(tvd.replace), 
                            text(matchFn.stringof ~": mismatch pattern #", a, ": ", tvd.pattern," expected: ",
                                    tvd.replace, " vs ", result));
                    }
                }
            }
        }
        debug(std_regex_test) writeln("!!! FReD bulk test done "~matchFn.stringof~" !!!");
    }

    run_tests!matchAll(); //thompson VM
}

}//version(unittest)
