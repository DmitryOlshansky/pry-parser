module dpick.buffer.traits;

/**
    Test if $(D T) follows Buffer concept. In particular the following code
    must compile for any $(D buf) of compatible type $(D T):
    ---
    import std.range;
    static assert(isForwardRange!T);
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));
        assert(buf.lookahead(2).length);
        assert(buf.lookbehind(2).length);
        static assert(isRandomAccessRange!(typeof(buf.lookahead(2))));
        static assert(isRandomAccessRange!(typeof(buf.lookbehind(2))));
        T m = buf.save;
        buf.popFront();
        auto s = buf.slice(m);
        alias S = typeof(s);
        static assert(isRandomAccessRange!S);
        static assert(is(ElementType!S : ubyte));
        buf = m;
        assert(buf.seek(-5));
        assert(buf.seek(5));
    }
    ---
*/
enum isBuffer(T) = __traits(compiles, (T buf){
    import std.range;
    static assert(isForwardRange!T);
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));
        assert(buf.lookahead(2).length);
        assert(buf.lookbehind(2).length);
        static assert(isRandomAccessRange!(typeof(buf.lookahead(2))));
        static assert(isRandomAccessRange!(typeof(buf.lookbehind(2))));
        T m = buf.save;
        buf.popFront();
        auto s = buf.slice(m);
        alias S = typeof(s);
        static assert(isRandomAccessRange!S);
        static assert(is(ElementType!S : ubyte));
        buf = m;
        assert(buf.seek(-5));
        assert(buf.seek(5));
    }
});

/**
    A mock implementation of Buffer concept.
    A type useful for compile-time instantiation checking.
*/
struct NullBuffer {
    ///InputRange troika.
    @property ubyte front(){ assert(0); }
    ///ditto
    @property bool empty(){ return true; }
    ///ditto
    void popFront(){ assert(0); }
    /**
        Create an independent copy of this range, a separate view
        reffering to the same buffer.
        This efectively pins down the buffer - the data stays in the buffer as long
        as there is a reference to it.
    */
    @property NullBuffer save(){ return this; }
    /**
        Take a slice starting from the current position to $(D n) bytes ahead.
        On success the size of sliсe is strictly equal $(D n)
        otherwise an empty slice is returned.
    */
    @property ubyte[] lookahead(size_t n){ return null; }
    /**
        Take a slice starting from $(D n) bytes behind to the current position.
        On success the size of slcie is strictly equal $(D n)
        otherwise an empty slice is returned.
    */
    @property ubyte[] lookbehind(size_t n){ return null; }
    ///Get offset of the position in the buffer of this range relative to $(D origin).
    ptrdiff_t tell(NullBuffer origin){ return 0; }
    /// Reset buffer state to an offset from the current position.
    /// Return indicates success of the operation.
    bool seek(ptrdiff_t){ return false; }
    /// Peek at the slice of buffer between of $(D m)
    /// and the current range's position. Must be random-access range of ubyte.
    ubyte[] slice(NullBuffer m){ return null; }
}

/**
    Test if can retain $(D Buffer)'s data slices directly, without taking a copy.
    The buffer type has to provide a slice as random access range
    having element type of $(D immutable(ubyte)).
*/
enum isZeroCopy(Buffer)  = isBuffer!Buffer && __traits(compiles, (Buffer buf){
    import std.range;
    auto m = buf.save();
    //slice may as well take only L-values
    alias SliceType = typeof(buf.slice(m));
    static assert(isRandomAccessRange!SliceType);
    static assert(is(ElementType!SliceType == immutable(ubyte)));
});

/**
    Tests if $(D Stream) follows the InputStream concept.
    In particular the following code must compiler for any Stream $(D s):
    ---
    (ref Stream s){
        ubyte[] buf;
        size_t len = s.read(buf);
        assert(s.eof);
        s.close();
    }
    ---
    $(D Stream) itself shall not have elaborate destructor and postblit
    and make no attempt at managing the liftime of the underlying resource.
    The ownership and deterministic release of resource
    are handled by a buffer range(s) working on this stream.

    In particular this allows class-based polymorphic $(D Stream) implementations
    to work w/o relying on nondeterministic GC finalization.

    See also $(DPREF2 _buffer, traits, NullInputStream).
*/
enum isInputStream(Stream) = __traits(compiles, (ref Stream s){
    ubyte[] buf;
    size_t len = s.read(buf);
    assert(s.eof);
    s.close();
});

///A do nothing implementation of InputStream concept.
struct NullInputStream
{
    /**
        Read some bytes to dest and return the amount acutally read.
        Upper limit is dest.length bytes.
    */
    size_t read(ubyte[] dest){ return 0; }
    /**
        Checks if reached the end of stream (file).
        Once eof returns true, all subsequent reads succeed but return 0.
    */
    @property bool eof(){ return true; }
    /// Close the underlying stream and free related resources.
    void close(){}
}

unittest
{
    static assert(isBuffer!NullBuffer);
    static assert(isInputStream!NullInputStream);
}