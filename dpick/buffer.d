module dpick.buffer;

import std.algorithm, std.range;

enum isBuffer(T) = __traits(compiles, (ref T buf){
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));        
        assert(buf[0] == v);
        assert(buf.has(2));
        auto m = buf.mark();
        buf.popFront();        
        auto s = buf.slice(m);
        static assert(isRandomAccessRange!(typeof(s)));
        static assert(is(ElementType!(typeof(s)) : ubyte));
    }
});

//can slice buffer's data directly, not copies nessary
enum isZeroCopy(Buffer)  = isBuffer!Buffer && __traits(compiles, (Buffer buf){ 
    auto m = buf.mark();
    //slice may as well take only L-values
    alias SliceType = typeof(buf.slice(m));
    static assert(is(SliceType : immutable(ubyte)[]));
});

/// Conceptual mark-slice Buffer with lookahead
struct BufferConcept {
    struct Range { }
    struct Mark { }
    ///InputRange troika
    @property ubyte front(){ assert(0); }
    ///ditto
    @property bool empty(){ assert(0); }
    ///ditto
    void popFront(){ assert(0); }
    /// lookahead from current position (extends buffer as required)
    ubyte opIndex(size_t idx){ assert(0); }
    /// check if the buffer has at least bytes left in it (so can use lookahead)
    @property bool has(size_t n){ assert(0); }
    /// instructs the underlying abstraction
    /// to keep a hidden 'absolute offset' to slice off later
    Mark mark(){ return Mark.init; }
    /// uses the same 'absolute offset' to peek 
    /// on slice from there to current position
    Range slice(ref Mark m){ assert(0); }
}


struct ArrayBuffer(T) {
    static struct Mark { size_t ofs; }
    @property ubyte front()
    in {  assert(!empty); }
    body { return data[cur]; }
    @property bool empty(){ return cur == data.length; }    
    void popFront()
    in {  assert(!empty); }
    body { cur++; }
    ubyte opIndex(size_t idx){ return data[cur+idx]; }
    @property bool has(size_t n){ return data.length  >= cur + n; }
    Mark mark(){ return Mark(cur); }
    T[] slice(Mark m){ return data[m.ofs..cur]; } 
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
    assert(buf[0] == 1);
    assert(buf.front == 1);
    buf.popFront();
    assert(buf.front == 2);
    assert(buf[0] == 2 && buf[1] == 3);
    auto m = buf.mark();
    foreach(_; 0..8)
        buf.popFront();
    assert(buf.empty);
    auto s = buf.slice(m);
    assert(s == [2, 3, 4, 5, 6, 7, 8, 9]);
}

struct GenericBuffer {
    enum chunkSize = 256; //a counter per 256 bytes
    static assert((chunkSize & (chunkSize-1)) == 0);
    static struct Mark {
        @disable this(this);
        ~this() {
            if(buf)
                buf.discard(ofs);
        }
        size_t ofs;
        GenericBuffer* buf;
    }
    this(size_t delegate(ubyte[]) readBlock, size_t bufSize) {
        read = readBlock;
        buffer = new ubyte[bufSize]; //TODO: revisit with std.allocator
        counters = new size_t[(bufSize+chunkSize-1)/chunkSize];
    }
    @property ubyte front()
    in {
        assert(!empty);
    }
    body {
        return buffer[cur];
    }
    @property bool empty() { return last && cur == buffer.length; }
    void popFront() {
        cur++; 
        if(cur == buffer.length && !last)
            refill();
    }
    ubyte opIndex(size_t idx) {
        if (buffer.length <= cur + idx) {
            assert(!last);
            refill();
        }
        return buffer[cur+idx];
    }
    @property bool has(size_t n) {
        return !last || buffer.length >= cur + n;
    }
    @property Mark mark() {
        return Mark(cur, &this);
    }
    ubyte[] slice(ref Mark m) {
        return buffer[m.ofs..cur];
    } 
    //start --> max(str, ofs) as counted by going through the ring
    void discard(size_t ofs) {
        auto blk = ofs / chunkSize;
        if(--counters[blk] == 0 && start < blk*chunkSize) {
            auto next = countUntil!"a != 0"(counters[blk..$]);
            start = next < 0 ? cur : chunkSize*(blk + next);
        }
    }

    private void refill() {
        if (start > chunkSize) {
            copy(buffer[start..$], buffer[0..$-start]);
            // copy counters
            auto blk = start/chunkSize;
            copy(counters[blk..$], counters[0..$-blk]);
            start = 0;
        }
        else { // compaction won't help
            buffer.length = buffer.length * 14 / 10;
            counters.length = (buffer.length + chunkSize - 1)/chunkSize;
        }
    }

    size_t delegate(ubyte[]) read;
    ubyte[] buffer; //
    size_t[] counters; //
    // start - first non-discarded ubyte
    // cur - index of current ubyte
    size_t start, cur;
    bool last; // no more bytes to read
}

static assert(isBuffer!(GenericBuffer));

auto genericBuffer(size_t delegate(ubyte[]) reader, size_t bufferSize=8096)
{
    return GenericBuffer(reader, bufferSize);
}