module dpick.buffer.traits;

///Test if T follows Buffer concept
enum isBuffer(T) = __traits(compiles, (ref T buf){
    import std.range;
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));        
        assert(buf[0] == v);
        assert(buf.lookahead(2));
        auto m = buf.mark();
        buf.popFront();        
        auto s = buf.slice(m);
        static assert(isRandomAccessRange!(typeof(s)));
        static assert(is(ElementType!(typeof(s)) : ubyte));
        buf.restore(m);
    }
});


/**
    A mock implementation of Buffer concept.
    A type useful for compile-time instantiation checking.
*/
struct NullBuffer {
    struct Range { }
    struct Mark { @disable this(this); }
    ///InputRange troika
    @property ubyte front(){ assert(0); }
    ///ditto
    @property bool empty(){ return true; }
    ///ditto
    void popFront(){ assert(0); }
    /// lookahead from current position (must call lookahead first)
    ubyte opIndex(size_t idx){ assert(0); }
    /// ensure that buffer has at least bytes left in it (so can use opIndex)
    @property bool lookahead(size_t n){ return false; }
    /// instructs the underlying abstraction
    /// to keep a hidden 'absolute offset' to slice off later
    Mark mark(){ return Mark.init; }
    /// Reset buffer state to previously marked position
    void restore(ref Mark ){ }
    /// Peek at slice from $(D m) to the current position
    Range slice(ref Mark m){ return Range.init; }
}

///Test if can slice $(D Buffer)'s data directly, without taking a copy
enum isZeroCopy(Buffer)  = isBuffer!Buffer && __traits(compiles, (ref Buffer buf){ 
    auto m = buf.mark();
    //slice may as well take only L-values
    alias SliceType = typeof(buf.slice(m));
    static assert(is(SliceType : immutable(ubyte)[]));
});

/**
    Tests if $(D Stream) follows the InputStream concept.
    See also $(LREF NullInputStream).
*/
enum isInputStream(T) = __traits(compiles, (ref T s){
    ubyte[] buf;
    size_t len = s.read(buf);
    assert(s.eof);
    s.close();
}) && !__traits(compiles, (T b){ T a = b; });


///A do nothing implementation of InputStream concept.
struct NullInputStream
{
    ///Non-copyable
    @disable this(this);
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
    /// Close underlying stream and free related resources.
    void close(){}
}
