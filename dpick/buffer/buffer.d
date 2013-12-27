module dpick.buffer.buffer;

import std.algorithm, std.range, std.traits;
import dpick.buffer.traits;

struct ArrayBuffer(T) {
    static struct Mark { size_t ofs; }
    @property ubyte front()
    in {  assert(!empty); }
    body { return data[cur]; }
    @property bool empty(){ return cur == data.length; }    
    void popFront()
    in {  
        assert(!empty); 
    }
    body { 
        cur++; 
    }
    T[] lookahead(size_t n){
        return data.length  < cur + n ? [] : data[cur .. cur + n]; 
    }
    T[] lookbehind(size_t n){
        return cur < n ? [] : data[cur - n .. cur]; 
    }
    void seek(Mark m, ptrdiff_t idx){ cur = m.ofs + idx; }
    void seek(Mark m){ cur = m.ofs; }
    void seek(ptrdiff_t offset){ cur += offset; }
    Mark mark(){ return Mark(cur); }
    ptrdiff_t tell(Mark m){
        return cur - m.ofs;
    }
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

///
auto buffer()(ubyte[] data)
{
    return ArrayBuffer!ubyte(data);
}

///
auto buffer(T)(T[] data)
    if(is(Unqual!T == ubyte))
{
    return ArrayBuffer!T(data);
}

///
auto buffer(T)(T[] data)
    if(is(Unqual!T == char))
{
    import std.string : representation;
    return buffer(data.representation);
}

static assert(isBuffer!(ArrayBuffer!ubyte));
static assert(isBuffer!(ArrayBuffer!(const(ubyte))));
static assert(isZeroCopy!(ArrayBuffer!(immutable(ubyte))));

unittest
{
    auto buf = buffer([1, 2, 3, 4, 5, 6, 7, 8, 9]);    
    auto luk = buf.lookahead(9);
    assert(buf.lookbehind(1) == null);
    assert(luk.length);
    assert(luk[0] == 1);
    assert(buf.front == 1);
    buf.popFront();
    assert(buf.front == 2);
    assert(buf.lookbehind(1)[0] == 1);
    luk = buf.lookahead(2);
    assert(luk[0] == 2 && luk[1] == 3);
    auto m = buf.mark();
    assert(buf.lookahead(8).length);
    foreach(_; 0..8)
        buf.popFront();
    assert(buf.empty);
    buf.seek(m, 8);
    assert(buf.empty);
    auto m2 = buf.mark();
    auto s = buf.slice(m);
    auto s2 = buf.slice(m2, m);
    assert(s == [2, 3, 4, 5, 6, 7, 8, 9]);
    assert(s ==  s2);
    assert(buf.slice(m2, m) == buf.slice(m, m2));
    assert(buf.lookahead(1) == null);
    buf.seek(m);
    luk = buf.lookahead(2);
    assert(buf.front == 2 && luk[1] == 3);
    assert(buf.slice(m2) == s);
    assert(iota(2, 10).equal(buf.slice(m, m2)));
    assert(buf.tell(m) == 0);
}

struct GenericBuffer(Input) 
    if(isInputStream!Input)
{
    static struct Mark {
        this(this){
            if(buf)
                buf.pin(this); //bump ref-count
        }
        ~this() {
            if(buf)
                buf.discard(pos);
        }
        ulong pos;
        GenericBuffer* buf;
    }

    this(Input inp, size_t chunk, size_t initial) {
        import core.bitop : bsr;
        assert((chunk & (chunk - 1)) == 0 && chunk != 0);
        static assert(bsr(1) == 0);
        chunkBits = bsr(chunk)+1;
        chunkMask = (1<<chunkBits)-1;
        input = move(inp);
        //TODO: revisit with std.allocator
        buffer = new ubyte[initial<<chunkBits];
        counters = new uint[initial + 1]; //extra counter for beyond last page
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

    @property ubyte[] lookahead(size_t n) {
        if (buffer.length >= cur + n)
            return buffer[cur .. cur + n];    
        if (last)
            return null;
        refill(n);
        //refill should get us the required length        
        return buffer.length >= cur + n ? buffer[cur .. cur + n] : null;
    }

    @property ubyte[] lookbehind(size_t n) {
        return cur >= n ? buffer[cur - n .. cur] : null;
    }

    private void refill(size_t extra=1)
    in {
        assert(!last);
    }
    body {
        //number of full blocks at front of buffer till first pinned by marks
        // or till 'cur' that is to be considered as pinned
        auto firstPage = counters.countUntil!(x => x != 0);
        auto start = firstPage < 0 ? cur & ~chunkMask : firstPage<<chunkBits;
        if (start >= extra + chunkMask) {
            copy(buffer[start .. $], buffer[0 .. $ - start]);
            if (firstPage >= 0) {
                copy(counters[firstPage .. $], counters[0 .. $ - firstPage]);
                counters[$ - firstPage .. $] = 0;
            }
            else
                counters[] = 0;
            mileage += start;
            cur -= start;
            //all after buffer.length - start is free space
            fillBuffer(buffer.length - start);
        }
        else {
            // compaction won't help
            // make sure we'd get at least extra bytes to read
            // rounded up to 2^^chunkBits
            auto oldLen = buffer.length;
            buffer.length = (max(cur + extra, buffer.length * 14 / 10) 
                + chunkMask) & ~chunkMask;
            counters.length = (buffer.length >> chunkBits) + 1;
            fillBuffer(oldLen);
            //no compaction - no mileage
        }
    }

    // read up to the end of buffer, starting at start; shorten on last read
    void fillBuffer(size_t start) {
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
        return cast(size_t)(absIdx >> chunkBits);
    }

    @property Mark mark() {
        auto m = Mark(mileage + cur, &this);
        pin(m);
        return m;
    }
    
    size_t tell(ref Mark m) {
        return cur - cast(size_t)(m.pos - mileage);
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

    void seek(ref Mark m, ptrdiff_t idx=0) {
        auto val = cast(size_t)(m.pos + idx - mileage);
        assert(val < buffer.length); //must be within the buffer
        cur = val;
    }

    void seek(ptrdiff_t ofs) {
        auto val = (cur + ofs);
        if (buffer.length > val) {
            cur = val;
            return;
        }
        //TODO: make sure it can skip the whole buffers if nothing's pinned
        assert(!last, "seek into unavailable part of buffer");
        refill(ofs);
        //refill should get us the required length
        // current position could have changed
        assert(cur + ofs < buffer.length, "seek into unavailable part of buffer");
        cur = val;
    }

    void pin(ref Mark m) {
        counters[page(m.pos - mileage)]++;
    }
    //
    void discard(ulong ofs) {
        counters[page(ofs - mileage)]--;
    }    
    Input input;
    ubyte[] buffer; //big enough to contain all present marks
    uint[] counters; //a counter per page (number of marks)
    size_t cur; //current position    
    size_t chunkBits, chunkMask;
    ulong mileage; //bytes discarded before curent buffer.ptr
    bool last; // no more bytes to read
}

static assert(isBuffer!(GenericBuffer!NullInputStream));

//TODO: tweak defaults
///
auto buffer(Input)(Input stream, size_t bufferSize=8*1024, size_t page=512)
    if(isInputStream!Input)
in {
    assert(bufferSize != 0 && ((bufferSize-1)&bufferSize) == 0);
    assert(page != 0 && ((page-1)&page) == 0);
    assert(page < bufferSize);
}
body {
    return GenericBuffer!Input(move(stream), page, bufferSize/page);
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
    assert(buf.lookbehind(10) == null);
    assert(buf.front == 10);
    assert(buf.lookahead(20));
    foreach(v; 10..40){
        auto luk = buf.lookahead(6);
        assert(buf.front == v, text(buf.front, " vs ", v));        
        assert(luk[2] == v+2);
        assert(luk[5] == v+5);
        buf.popFront();
    }
    {
        auto m = buf.mark();
        auto m2 = buf.mark();
        foreach(v; 40..70) {
            assert(buf.front == v);
            assert(buf.tell(m) ==  v - 40);
            buf.popFront();
        }
        auto lukB = buf.lookbehind(30);
        assert(lukB.equal(iota(40, 70)));
        buf.seek(-30);
        auto lukA = buf.lookahead(30);
        assert(lukB == lukB);
        buf.seek(m, 30);
        assert(buf.slice(m, m2).empty);
        assert(equal(buf.slice(m2), buf.slice(m)));
        assert(equal(buf.slice(m), iota(40, 70)));
    }
    auto m = buf.mark();
    assert(equal(&buf, iota(70, 100)));
    buf.seek(m);
    assert(buf.tell(m) ==  0);
    assert(equal(&buf, iota(70, 100)));
    assert(equal(buf.slice(m), iota(70, 100)));
    assert(buf.lookahead(10) == null);
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
    auto luk = buf.lookahead(4);
    if(luk.length) {
        //do away with direct indexing and no range checks
    L_fastSwitch:
        switch(msbs){
        foreach(n; TypeTuple!(2, 3, 4))
        {
        case n:
            ret |= (c & leadMask!n) << 6*(n-1);
            foreach(v; Sequence!(1, n))
            {
                uint x = luk[v];
                if (x < 0x80)
                    badUtf8();
                ret |= (x  & 0x3F) << 6*(n-v-1);
            }
            buf.seek(n); //fast-forward by n
            break L_fastSwitch;
        }
        case 1: case 5: case 6: case 7:
        default:
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
                uint x = buf.front;
                if (x < 0x80)
                    badUtf8();
                ret |= (x & 0x3F) << 6*(n-v-1);
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
    foreach(msg; TypeTuple!("QЯऄ𫟖", "𫟖", "ऄ", "Г", "\u00c2\u2200\u00c3\u2203.")){
        auto buf = buffer(cast(immutable(ubyte)[])msg);
        auto m = msg;
        while(!m.empty){
            assert(decodeUtf8(buf) == m.front);
            m.popFront();
        }
    }
    import std.exception;
    //decode fail case
    alias fails = TypeTuple!("\xC1", "\x80\x00", "\xCF\x79", 
        "\xFF\x00\0x00\0x00\x00", "\x80\0x00\0x00\x00", "\xCF\x00\0x00\0x00\x00");
    foreach(msg; fails){
        assert(collectException((){
            auto buf = buffer(msg);
            decodeUtf8(buf);
        }()));
    }
}
