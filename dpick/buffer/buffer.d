module dpick.buffer.buffer;

import std.algorithm, std.range, std.traits;
import dpick.buffer.traits;

/**
    Buffer for wrapping arrays, all operations work directly on the array
    offering no extra overhead.
*/
struct ArrayBuffer(T) {
    @property ubyte front()
    in {
        assert(!empty);
    }
    body {
        return data[cur];
    }

    @property bool empty(){ return cur == data.length; }

    @property auto save(){ return this; }

    void popFront()
    in {
        assert(!empty);
    }
    body {
        cur++;
    }

    void seek(ptrdiff_t offset){ cur += offset; }

    ptrdiff_t tell()(auto ref ArrayBuffer m){ return cur - m.cur; }

    T[] slice()(auto ref ArrayBuffer m){
        return m.cur <= cur ? data[m.cur .. cur] : data[cur .. m.cur];
    }

    T[] lookahead(size_t n){
        return data.length  < cur + n ? [] : data[cur .. cur + n];
    }

    T[] lookbehind(size_t n){
        return cur < n ? [] : data[cur - n .. cur];
    }
private:
    T[] data;
    size_t cur;
}

/**
     Wrap an array as buffer.
     If the original array had immutable elements then the resulting
     buffer supports zero-copy slicing.
*/
auto buffer()(ubyte[] data)
{
    return ArrayBuffer!ubyte(data);
}

/// ditto
auto buffer(T)(T[] data)
    if(is(Unqual!T == ubyte))
{
    return ArrayBuffer!T(data);
}

/// ditto
auto buffer(T)(T[] data)
    if(is(Unqual!T == char))
{
    import std.string : representation;
    return buffer(data.representation);
}

unittest
{
    static assert(isForwardRange!(ArrayBuffer!ubyte));
    static assert(isBuffer!(ArrayBuffer!ubyte));
    static assert(isBuffer!(ArrayBuffer!(const(ubyte))));
    static assert(isZeroCopy!(ArrayBuffer!(immutable(ubyte))));

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
    auto m = buf.save();
    assert(buf.lookahead(8).length);
    foreach(_; 0..8)
        buf.popFront();
    assert(buf.empty);
    buf = m;
    buf.seek(8);
    assert(buf.empty);
    auto m2 = buf.save();
    auto s = buf.slice(m);
    auto s2 = m2.slice(m);
    assert(s == [2, 3, 4, 5, 6, 7, 8, 9]);
    assert(s ==  s2);
    assert(m.slice(m2) == m2.slice(m));
    assert(buf.lookahead(1) == null);
    buf = m;
    luk = buf.lookahead(2);
    assert(buf.front == 2 && luk[1] == 3);
    assert(buf.slice(m2) == s);
    assert(iota(2, 10).equal(m.slice(m2)));
    assert(buf.tell(m) == 0);
}

//@@@BUG@@@ 11098 (should be inside of GenericBufferRef)
private struct NodeT(Impl){
    size_t pos, cnt;
    Impl* buf;
    NodeT!Impl* next, prev; //circular doubly-linked list
}

private static void apply(alias fn, Impl)(NodeT!Impl* start)
{
    NodeT!Impl* p = start;
    if (p)
        do{
            fn(p);
            p = p.next;
        }while(p != start);
}

//debug tool
private void printRefs(Impl)(NodeT!Impl* node)
{
    import std.stdio;
    apply!(p => writefln("%x pos=%d, cnt=%d, prev=%x, next=%x", 
        p, p.pos, p.cnt, p.prev, p.next))(node);
}


struct GenericBufferRef(Impl)
{
   @property ubyte front()
    in {
        assert(!empty);
    }
    body {
        //empty & popFront make sure it's accessible
        return impl.window[ptr.pos];
    }

    @property bool empty() {
        assert(ptr);
        return impl.last && impl.window.length == ptr.pos;
    }

    void popFront() {
        ptr.pos++;
        if(ptr.pos == impl.window.length){
            if (!impl.last)
                read(1);
        }
    }

    auto save(){
        This copy;
        copy.ptr = fork();
        return copy;
    }

    ptrdiff_t tell(ref This r) {
        return ptr.pos - r.ptr.pos;
    }

    ubyte[] slice(ref This r) {
        return ptr.pos <= r.ptr.pos ? 
                impl.window[ptr.pos .. r.ptr.pos] : 
                impl.window[r.ptr.pos .. ptr.pos];
    }

    void seek(ptrdiff_t ofs) {
        bool seekable = ensureSeekable(ofs);
        assert (seekable); //TODO: switch to bool seek(ptrdiff_t ofs) 
        //if (seekable)
        ptr.pos += ofs;
        //return seekable;
    }

    //
    ubyte[] lookahead(size_t n){
        return ensureSeekable(n) ? impl.window[ptr.pos .. ptr.pos + n] : null;
    }

    //
    ubyte[] lookbehind(size_t n){
        return ensureSeekable(-cast(ptrdiff_t)n) ?
            impl.window[ptr.pos .. ptr.pos + n] : null;
    }

    //created a new (shared) copy of this ref
    this(this){        
        if (ptr)
            ptr.cnt++;
    }

    //lvalue - new copy of 'that' reference
    ref opAssign(ref This that){
        dispose();
        ptr = that.ptr;
        if (ptr)
            ptr.cnt++;
        return this;
    }

    //rvalue - just steal 'that' reference
    ref opAssign(This that){
        dispose();
        ptr = that.ptr;
        that.ptr = null;
        return this;
    }

    ~this(){
        dispose();
    }
private:
    alias Node = NodeT!Impl;

    static auto alloc(){
        Node* r;
        if (freeList){
            r = freeList;
            freeList = freeList.next;
        }
        else
            r = new Node();
        return r;
    }

    static auto create(){
        auto q = alloc();
        q.cnt = 1;
        //link in a ring of 1 element
        q.next = q.prev = q;
        return q;
    }

    //create a new independent reference, as an exact copy of n
    auto fork() {
        auto q = alloc();
        q.cnt = 1;
        q.buf = ptr.buf;
        q.pos = ptr.pos;
        //insert before 'this' reference
        if (ptr == ptr.next) { //one element ring
            q.next = q.prev = ptr;
            ptr.next = ptr.prev = q;
        }
        else {
            //   ptr.prev<--> ptr <--> ptr.next
            //   ptr.prev<--> q <---> ptr <---> ptr.next
            q.next = ptr;
            q.prev = ptr.prev;
            ptr.prev.next = q;
            ptr.prev = q;
        }
        ptr.prev = q;
        return q;
    }

    //kill this reference - decrement refcount
    //and as needed:
    //  remove from the ring
    //  (for the last link) dispose the buffer
    void dispose() {
        if (ptr) {
            if (--ptr.cnt == 0){
                // the only link in chain?
                if (ptr.next == ptr) {
                    //yes - destroy the buffer
                    assert(ptr.prev == ptr);
                    impl.dispose();
                }
                else {
                    //not - unlink from the ring
                    auto next = ptr.next;
                    auto prev = ptr.prev;
                    next.prev = prev;
                    prev.next = next;
                    //put this node into the free list
                    ptr.prev = null;
                    ptr.next = freeList;
                    freeList = ptr;
                }
            }
            ptr = null;
        }
    }

    // read data to reach the index at offset of 'ofs'
    bool ensureSeekable(ptrdiff_t ofs){
        ptrdiff_t val = ptr.pos + ofs;
        //within the buffer?
        if (val < 0)
            return false;
        if (impl.window.length > val)
            return true;
        if (impl.last)
            return false;
        //must have at least 1 byte before the end of window
        read(val - impl.window.length + 1);
        // current position may have changed during the read
        // but it must end up inside of the buffer
        return ptr.pos + ofs < impl.window.length;
    }

    // a wrapper to read no less then n new bytes
    void read(size_t n){
        size_t maxDiscard = impl.window.length;
        apply!(p => maxDiscard = min(p.pos, maxDiscard))(ptr);
        //call adjustPos hook if it was a buffer compaction
        size_t discarded = impl.load(maxDiscard, n);
        if (discarded) {
            apply!(p => p.pos -= discarded)(ptr);
        }
    }

    //tivial constructor - take ownership of a unique buffer impl
    this(Impl* buffer){
        ptr = create();
        ptr.buf = buffer;
        ptr.pos = 0;
        ptr.next = ptr;
        ptr.prev = ptr;
    }

    @property ref impl(){ return *ptr.buf; }

    alias This = typeof(this);
    Node* ptr;
    //TODO: use std.allocator
    static Node* freeList;
}

struct FakeBufferImpl
{
    @property ubyte[] window(){ return null; }

    // !=0 on compaction, returns number of bytes were discarded
    size_t load(size_t discard, size_t toLoad)
    {
        return 0;
    }
    
    @property bool last(){ return true; }

    void dispose(){ closed++; }
private:
    int closed;
}

//test ring of references scheme
unittest
{
    alias Buf = GenericBufferRef!FakeBufferImpl;
    auto impl = new FakeBufferImpl;
    {   
        //w/o but save
        auto buf = Buf(impl);
        //2 postblits
        auto buf2 = buf;
        auto buf3 = buf;
        //l-value
        buf2 = (buf3 = buf);
        //NRVO r-value
        buf2 = (){ auto x = buf; return x; }();
    }
    assert(impl.closed == 1);
    {
        //with save
        auto buf = Buf(impl);
        //postblit + save
        auto buf2 = buf.save;
        //r-value + save
        buf2 = buf2.save;
        //r-value + more save
        auto buf3 = buf2.save.save.save;
        //l-value + save
        buf = (buf3 = buf.save);
        //NRVO r-value + save
        buf = (){ auto x = buf2.save; return x; }();
    }
}

/*
    A buffer implementation that works with any type compatible
    with InputStream concept. The implementation uses a 
    plain array buffer internally. 

    On each request to load another X bytes it then chooses between 
    compaction and expansion of the current window to satisfy
    the request.
    
*/
struct GenericBufferImpl(Input)
    if(isInputStream!Input)
{
    this(Input inp, size_t minHistory, size_t chunk, size_t initial) {
        import core.bitop : bsr;
        assert((chunk & (chunk - 1)) == 0 && chunk != 0);
        static assert(bsr(1) == 0);
        auto pageBits = bsr(chunk)+1;
        pageMask = (1<<pageBits)-1;
        input = move(inp);
        history = (minHistory + pageMask) & ~pageMask; //round up to page
        //TODO: revisit with std.allocator
        buffer = new ubyte[initial<<pageBits];
        fillBuffer(0);
    }

    @disable this(this);

    //
    @property auto window(){
        return buffer;
    }

    // !=0 on compaction, returns number of bytes were discarded
    size_t load(size_t discard, size_t toLoad)
    in {
        assert(!last);
    }
    body {
        //number of full blocks at front of buffer till first pinned by marks
        // or till 'cur' that is to be considered as pinned
        auto start =  discard > history ? discard - history : 0;
        start = start & ~pageMask; //round down to chunk
        //TODO: tweak condition w.r.t. cost-benefit of compaction vs realloc
        if (start >= toLoad + pageMask) {
            // toLoad + pageMask -> at least 1 page, no less then toLoad
            copy(buffer[start .. $], buffer[0 .. $ - start]);
            //all after buffer.length - start is free space
            fillBuffer(buffer.length - start);
            return start;
        }
        else {
            // compaction won't help
            // make sure we'd get at least toLoad bytes to read
            // rounded up to 2^^chunkBits
            //TODO: tweak grow rate formula
            auto oldLen = buffer.length;
            auto newLen = max(oldLen + toLoad, oldLen * 14 / 10);
            newLen = (newLen + pageMask) & ~pageMask; //round up to page
            buffer.length = newLen;
            fillBuffer(oldLen);
            return 0;
        }
    }
    
    @property bool last(){ return ended; }

    void dispose(){
        input.close();
    }
private:
    // read up to the end of buffer, starting at start; shorten on a short-read
    void fillBuffer(size_t start) {
        size_t got = input.read(buffer[start .. $]);
        if (got + start < buffer.length) {
            buffer = buffer[0 .. got + start];
            if(input.eof) //true end of stream
                ended = true;
            else //short-read, e.g. on a socket, port - reuse the array
                buffer.assumeSafeAppend(); 
        }
    }
    ubyte[] buffer; //big enough to contain all present marks 
    bool ended; // no more bytes to read  TODO: merge as bit-field with 'history'
    size_t pageMask; //bit mask - used for fast rounding to multiple of page    
    size_t history; // minimal amount of bytes to keep during compaction
    Input input;
}

/**
    Creates a bufer range that takes ownership of $(D stream) input stream.
    Tweakable parameters include initial buffer size,
    a block size of the buffer,
    and minimal history (in bytes) to keep for lookbehind during buffering.
*/
auto buffer(Input)(Input stream, size_t minHistory=32,
    size_t bufferSize=8*1024, size_t page=512)
    if(isInputStream!Input)
in {
    assert(bufferSize != 0 && ((bufferSize-1)&bufferSize) == 0);
    assert(page != 0 && ((page-1)&page) == 0);
    assert(page < bufferSize);
    assert(minHistory < bufferSize);
}
body {
    //TODO: allocators, allocators everywhere!
    alias Buf = GenericBufferImpl!Input;
    Buf* impl = new Buf(move(stream), minHistory, page, bufferSize/page);
    //pass ownership to a new ref-counted buffer-range
    return GenericBufferRef!Buf(impl);
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
    auto buf = buffer(ChunkArray(arr), 2, 4, 2);
    assert(!buf.empty);
    assert(buf.lookbehind(10) == null);
    assert(buf.front == 10);

    assert(buf.lookahead(20).length);
    foreach(v; 10..40){
        auto luk = buf.lookahead(6);
        assert(buf.front == v, text(buf.front, " vs ", v));
        assert(luk[2] == v+2);
        assert(luk[5] == v+5);
        buf.popFront();
    }
    /*
    assert(buf.lookbehind(2).equal([38, 39]));
    {
        auto m = buf.save;
        auto m2 = buf.save;
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
        buf = m;
        buf.seek(30);
        assert(m.slice(m2).empty);
        assert(equal(buf.slice(m2), buf.slice(m)));
        assert(equal(buf.slice(m), iota(40, 70)));
    }
    auto m = buf.save;
    assert(equal(&buf, iota(70, 100)));
    buf = m;
    assert(buf.tell(m) ==  0);
    assert(equal(&buf, iota(70, 100)));
    assert(equal(buf.slice(m), iota(70, 100)));
    assert(buf.lookahead(10) == null);*/
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
