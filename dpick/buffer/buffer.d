module dpick.buffer.buffer;

import std.algorithm, std.range;
import dpick.buffer.traits;

struct ArrayBuffer(T) {
    static struct Mark { size_t ofs; }
    @property ubyte front()
    in {  assert(!empty); }
    body { return data[cur]; }
    @property bool empty(){ return cur == data.length; }    
    void popFront()
    in {  assert(!empty); }
    body { cur++; }
    ubyte opIndex(size_t idx){ return data[cur + idx]; }
    @property bool lookahead(size_t n){ return data.length  >= cur + n; }
    void restore(Mark m){ cur = m.ofs; }
    Mark mark(){ return Mark(cur); }
    T[] slice(Mark m){
        return m.ofs <= cur ? data[m.ofs .. cur] : data[cur .. m.ofs];
    }
    T[] slice(Mark m1, Mark m2){
        return m1.ofs <= m2.ofs ? data[m1.ofs .. m2.ofs] : data[m2.ofs .. m1.ofs];
    }
private:
    T[] data;
    size_t cur;
}

auto buffer()(ubyte[] data)
{
    return ArrayBuffer!ubyte(data);
}

auto buffer(T)(T[] data)
    if(is(T : ubyte))
{
    return ArrayBuffer!T(data);
}

static assert(isBuffer!(ArrayBuffer!ubyte));
static assert(isBuffer!(ArrayBuffer!(const(ubyte))));
static assert(isZeroCopy!(ArrayBuffer!(immutable(ubyte))));

unittest
{
    auto buf = buffer([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    assert(buf.lookahead(9));
    assert(buf[0] == 1);
    assert(buf.front == 1);
    buf.popFront();
    assert(buf.front == 2);
    assert(buf[0] == 2 && buf[1] == 3);
    auto m = buf.mark();
    assert(buf.lookahead(8));
    foreach(_; 0..8)
        buf.popFront();
    assert(buf.empty);
    auto m2 = buf.mark();
    auto s = buf.slice(m);
    auto s2 = buf.slice(m2, m);
    assert(s == [2, 3, 4, 5, 6, 7, 8, 9]);
    assert(s ==  s2);
    assert(buf.slice(m2, m) == buf.slice(m, m2));
    buf.restore(m);
    assert(buf.front == 2 && buf[1] == 3);
    assert(buf.slice(m2) == s);
}

//pinning implemented as r-b tree
struct PinningRBT {
    import std.container;
    //TODO: better set structure (preferably tunned for small sets)
    RedBlackTree!(size_t) marks;
    //AA mark -> num of marks to the same chunk
    uint[size_t] counts;
    void add(size_t blk) {        
        if(blk in counts) //hash is O(1)
            counts[blk]++;
        else {
            marks.insert(blk);
            counts[blk] = 1;
        }
    }
    void remove(size_t blk) {
        if(--counts[blk] == 0) {
            marks.remove(marks.equalRange(blk));
            counts.remove(blk);
        }
    }
    @property empty(){ return marks.empty; }
    @property size_t lowerBound(){ return marks.front; }
}

//lack of 0-arg ctors
auto pinningRBT()
{
    import std.container;
    PinningRBT ret;
    ret.marks = new RedBlackTree!(size_t);
    return ret;
}

struct GenericBuffer(Input) 
    if(isInputStream!Input)
{
    static struct Mark {
        @disable this(this);
        ~this() {
            if(buf)
                buf.discard(pos);
        }
        ulong pos;
        GenericBuffer* buf;
    }

    this(Input inp, size_t chunk, size_t initial) {
        assert((chunk & (chunk - 1)) == 0);
        chunkSize = chunk;
        input = move(inp);
        buffer = new ubyte[initial * chunkSize]; //TODO: revisit with std.allocator
        pinning = pinningRBT();
        fillBuffer(0);
    }

    @disable this(this);

    @property ubyte front()
    in {
        assert(!empty);
    }
    body {
        return buffer[cur];
    }

    @property bool empty() { 
        return last && cur == buffer.length; 
    }

    void popFront() {
        cur++; 
        if(cur == buffer.length && !last)
            refill();
    }

    //TODO: could do an optimized popFrontN

    ubyte opIndex(size_t idx) {
        assert(cur + idx < buffer.length, 
            "Buffer overrun while indexing - was lookahead called?");
        return buffer[cur + idx];
    }

    @property bool lookahead(size_t n) {
        if (buffer.length < cur + n) {
            if (last)
                return false;
            refill(n);
            //refill should get us the required length        
            return buffer.length >= cur + n;
        }
        return true;
    }    

    private void refill(size_t extra=1)
    in {
        assert(!last);
    }
    body {
        //number of full blocks at front of buffer till first pinned by marks
        // or till 'cur' that is to be considered as pinned
        auto start = pinning.empty ? cur & ~(chunkSize-1) : 
                chunkSize*(pinning.lowerBound - cast(size_t)(mileage/chunkSize));
        if (start >= extra + chunkSize-1) {
            copy(buffer[start..$], buffer[0 .. $ - start]);
            mileage += start;
            cur -= start;
            //all after buffer.length - start is free space
            fillBuffer(buffer.length - start);
        }
        else {
            // compaction won't help
            // make sure we'd get at least extra bytes to read
            auto oldLen = buffer.length;
            buffer.length = max(cur + extra, 
                buffer.length * 14 / 10  & ~(chunkSize-1));
            fillBuffer(oldLen);
            //no compaction - no mileage
        }
    }

    // read up to the end of buffer, starting at start; shorten on last read
    void fillBuffer(size_t start)
    {
        size_t got = input.read(buffer[start .. $]);
        if (got + start < buffer.length) {            
            buffer = buffer[0 .. got + start];
            if(input.eof)
                last = true;
            else
                buffer.assumeSafeAppend();
        }
    }

    size_t offset()(ref Mark m) {
        return cast(size_t)(m.pos - mileage);
    }

    size_t page()(ulong absIdx) {
        return cast(size_t)(absIdx / chunkSize);
    }

    @property Mark mark() {
        auto m = Mark(mileage + cur, &this);
        pinning.add(page(m.pos));
        return m;
    }

    ubyte[] slice(ref Mark m) {
        auto ofs = offset(m);
        return ofs <= cur ? buffer[ofs .. cur] : buffer[cur .. ofs];
    }

    ubyte[] slice(ref Mark m1, ref Mark m2) {
        auto ofs1 = offset(m1);
        auto ofs2 = offset(m2);
        return ofs1 <= ofs2 ? buffer[ofs1 .. ofs2] : buffer[ofs1 .. ofs2];
    }

    void restore(ref Mark m) {
        cur = cast(size_t)(m.pos - mileage);
    }

    //
    void discard(ulong ofs) {
        pinning.remove(page(ofs));        
    }
    PinningRBT pinning;
    Input input;
    ubyte[] buffer; //big enough to contain all present marks
    size_t cur; //current position    
    size_t chunkSize;
    ulong mileage; //bytes discarded before curent buffer.ptr
    bool last; // no more bytes to read
}

static assert(isBuffer!(GenericBuffer!NullInputStream));

//TODO: tweak defaults
auto buffer(Input)(Input stream, size_t chunk=512, size_t n=16)
    if(isInputStream!Input)
{
    return GenericBuffer!Input(move(stream), chunk, n);
}

unittest
{
    static struct ChunkArray {
        @disable this(this);
        this(ubyte[] src) {
            leftover = src;
        }
        size_t read(ubyte[] dest){
            auto toCopy = min(leftover.length, dest.length);
            dest[0..toCopy] = leftover[0..toCopy];
            leftover = leftover[toCopy..$];
            return toCopy;
        }
        @property bool eof(){ return leftover.length == 0; }
        void close(){}
        ubyte[] leftover;
    }
    static assert(isInputStream!ChunkArray);
    
    import std.conv;
    ubyte[] arr = iota(cast(ubyte)10, cast(ubyte)100).array;
    //simple stream - slice a piece of array 
    auto buf = buffer(ChunkArray(arr), 4, 2);
    assert(!buf.empty);
    assert(buf.front == 10);
    assert(buf.lookahead(20));
    foreach(v; 10..40){
        assert(buf.lookahead(6));
        assert(buf.front == v, text(buf.front, " vs ", v));
        assert(v + 2 >= 40 || buf[2] == v+2);
        assert(v + 5 >= 40 || buf[5] == v+5);
        buf.popFront();
    }
    {
        auto m = buf.mark();
        auto m2 = buf.mark();
        foreach(v; 40..70) {
            assert(buf.front == v);
            buf.popFront();
        }
        assert(buf.slice(m, m2).empty);
        assert(equal(buf.slice(m2), buf.slice(m)));
        assert(equal(buf.slice(m), iota(40, 70)));
    }
    auto m = buf.mark();
    assert(equal(&buf, iota(70, 100)));
    buf.restore(m);
    assert(equal(&buf, iota(70, 100)));
    assert(equal(buf.slice(m), iota(70, 100)));
}

//Decoding on buffers

//generate code for TypeTuple(S, S+1, S+2, ... E)
@system string ctGenSeq(int S, int E)
{
    import std.conv;
    string s = "alias TypeTuple!(";
    if(S < E)
        s ~= to!string(S);
    for(int i = S+1; i < E;i++)
    {
        s ~= ", ";
        s ~= to!string(i);
    }
    return s ~") Sequence;";
}

//alias to TypeTuple(S, S+1, S+2, ... E)
template Sequence(int S, int E)
{
    import std.typetuple;
    mixin(ctGenSeq(S,E));
}

void badUtf8()
{
    import std.utf;
    throw new UTFException("Invalid UTF-8 sequence");
    assert(0);
}

dchar decodeUtf8(Buffer)(ref Buffer buf)
    if(isBuffer!Buffer)
{
    assert(!buf.empty);
    //comma operator to make DMD inliner happy
    auto c = buf.front;
    return c & 0x80 ? decodeUtf8Impl(buf) : (buf.popFront(), c);
}

dchar decodeUtf8Impl(Buffer)(ref Buffer buf)
{
    import std.typetuple;
    enum leadMask(size_t size) = (cast(size_t)1<<(8 - size))-1;
    import core.bitop;
    ubyte c = buf.front;
    immutable msbs = 7 - bsr(~c);
    dchar ret = 0;
    if(buf.lookahead(4)) {
        //do away with direct indexing and no range checks
    L_fastSwitch:
        switch(msbs){
        foreach(n; TypeTuple!(2, 3, 4))
        {
        case n:
            ret |= (c & leadMask!n) << 6*(n-1);
            foreach(v; Sequence!(1, n))
            {                
                uint x = buf[v] & 0x3F;
                if(!x)
                    badUtf8(); //overlong UTF-8
                ret |= x << 6*(n-v-1);
            }
            buf.popFrontN(n);
            break L_fastSwitch;
        }
        case 1: case 5: case 6: case 7:
        default:
            import std.stdio;
            writeln(msbs);
            badUtf8();
        }
    }
    else {
    L_slowSwitch:
        switch(msbs){
        foreach(n; TypeTuple!(2, 3, 4))
        {
        case n:
            ret |= (c & leadMask!n) << 6*(n-1);
            buf.popFront();
            foreach(v; Sequence!(1, n))
            {
                if(buf.empty)
                    badUtf8();
                uint x = buf.front & 0x3F;
                if(!x)
                    badUtf8();
                ret |= x << 6*(n-v-1);
                buf.popFront();
            }
            break L_slowSwitch;
        }
        case 1: case 5: case 6: case 7:
        default:
            badUtf8();
        }
    }
    return ret;
}

unittest
{
    import std.typetuple;
    foreach(msg; TypeTuple!("QЯऄ𫟖", "𫟖", "ऄ", "Г")){
        auto buf = buffer(cast(immutable(ubyte)[])msg);
        auto m = msg;
        while(!m.empty){
            assert(decodeUtf8(buf) == m.front);
            m.popFront();
        }
    }
}