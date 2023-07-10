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

	SimpleStream save() {
		auto t = this;
		return t;
	}

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
