module pry.stream;

import pry.traits;
import std.range.primitives, std.traits, std.utf;

struct SimpleStream(S)
if(isSomeString!S) {
private:
	size_t _index;
	S _range;
public:
	alias Range = S;
	static struct Error {
		size_t location;
		string reason;
	}

	dchar front(){
		size_t t = _index;
		return decode(_range, t);
	}

	this(S source){
		_range = source;
		_index = 0;
	}

	void popFront(){  _index += stride(_range, _index); }

	@property bool empty(){ return _index == _range.length; }

	S slice(size_t i){ return _range[i .. _index]; }

	size_t mark(){ return _index; }

	void restore(size_t i){ _index = i; }

	@property size_t location() {
		return _index;
	}
}

static assert(isStream!(SimpleStream!string));

/// Wrap a given slicable random access range into a SimpleStream.
auto stream(Range)(Range range)
if(isRandomAccessRange!Range || isSomeString!Range) {
	return SimpleStream!Range(range);
}

struct SkipWhitespace(Stream)
if(isStream!Stream){
private:
	import std.uni;
	Stream s;
	void skip(){
		while(!s.empty && isWhite(s.front)) s.popFront();
	}
public:
	alias Error = Stream.Error;
	alias Range = Stream.Range;
	bool skipWs = true;

	this(Stream stream){
		s = stream;
		skip();
	}

	dchar front(){
		return s.front;
	}

	void popFront(){
		s.popFront();
		if(skipWs) skip();
	}

	@property bool empty(){ return s.empty; }

	Range slice(size_t i){ return s.slice(i); }

	size_t mark(){ return s.mark(); }

	void restore(size_t i){ s.restore(i); }

	@property size_t location() {
		return s.location();
	}
}

/// Wrap a stream with skip
auto skipWs(S)(S stream)
if(isStream!S && is(ElementType!S : dchar)){
	return SkipWhitespace!S(stream);
}

unittest {
	import std.algorithm;
	auto s = " a b   c ".stream.skipWs;
	assert(equal(s, "abc"));
}