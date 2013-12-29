module dpick.buffer.traits;

/**
    Test if T follows Buffer concept. In particular the following code 
    must compile for any $(D buf) of compatible type:
    ---
    import std.range;
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));                
        assert(buf.lookahead(2).length);
        assert(buf.lookbehind(2).length);
        static assert(isRandomAccessRange!(typeof(buf.lookahead(2))));
        static assert(isRandomAccessRange!(typeof(buf.lookbehind(2))));
        auto m = buf.mark();
        buf.popFront();
        auto m2 = buf.mark();
        auto s = buf.slice(m);
        auto s2 = buf.slice(m, m2);
        alias S = typeof(s);
        alias S2 = typeof(s2);
        static assert(isRandomAccessRange!S);
        static assert(isRandomAccessRange!S2);
        static assert(is(ElementType!S : ubyte));
        static assert(is(ElementType!S2 : ubyte));
        buf.seek(m);
        buf.seek(m, -5);
        buf.seek(m, 5);
        buf.seek(5);
        buf.seek(-5);
    }
    ---
*/
enum isBuffer(T) = __traits(compiles, (ref T buf){
    import std.range;
    if(!buf.empty) {
        auto v = buf.front;
        static assert(is(typeof(v) : ubyte));                
        assert(buf.lookahead(2).length);
        assert(buf.lookbehind(2).length);
        static assert(isRandomAccessRange!(typeof(buf.lookahead(2))));
        static assert(isRandomAccessRange!(typeof(buf.lookbehind(2))));
        auto m = buf.mark();
        buf.popFront();
        auto m2 = buf.mark();
        auto s = buf.slice(m);
        auto s2 = buf.slice(m, m2);
        alias S = typeof(s);
        alias S2 = typeof(s2);
        static assert(isRandomAccessRange!S);
        static assert(isRandomAccessRange!S2);
        static assert(is(ElementType!S : ubyte));
        static assert(is(ElementType!S2 : ubyte));
        buf.seek(m);
        buf.seek(m, -5);
        buf.seek(m, 5);
        buf.seek(5);
        buf.seek(-5);
    }
});

/**
    A mock implementation of Buffer concept.
    A type useful for compile-time instantiation checking.
*/
struct NullBuffer {
    ///Slice of this buffer, a random-access range of ubyte.
    struct Range { }
    ///A type representing a position in this buffer (a savepoint).
    struct Mark { @disable this(this); }
    ///InputRange troika.
    @property ubyte front(){ assert(0); }
    ///ditto
    @property bool empty(){ return true; }
    ///ditto
    void popFront(){ assert(0); }   
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
    /**
        Instruct the underlying buffer abstraction
        to keep a vantage point to slice the data later.
        In particular as long as returned mark is not destroyed
        the data following after it is kept intact during any potential internal 
        buffering.
    */
    Mark mark(){ return Mark.init; }
    ///Get offset of the current position in the buffer relative to $(D origin).
    ptrdiff_t tell(ref Mark origin){ return 0; }
    /// Reset buffer state to an offset from the current position.
    void seek(ptrdiff_t){ }
    /// Reset buffer state to a previously marked position.
    void seek(ref Mark ){ }
    /**
         Reset buffer state to a position offseted by $(D diff) from 
         a previously marked position.
    */
    void seek(ref Mark, ptrdiff_t diff){ }
    /// Peek at the slice of buffer from $(D m) to the current position.
    Range slice(ref Mark m){ return Range.init; }
    /// Peek at the slice of buffer from $(D m1) to $(D m2).
    Range slice(ref Mark m1, ref Mark m2){ return Range.init; }
}

/**
    Test if can slice $(D Buffer)'s data directly, without taking a copy.
    The buffer type has to provide slice
*/
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
