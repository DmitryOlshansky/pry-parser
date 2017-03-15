module pry.atoms;

import pry.stream, pry.traits;
import std.conv, std.range.primitives;

template parsers(Stream)
{
	struct Tk(alias c)
	if(is(typeof(c) : ElementType!Stream)) {
		static immutable msg = "expected '" ~ to!string(c)~"'";

		bool parse(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err){
			if(stream.empty) {
				err.location = stream.location;
				err.reason = "unexpected end of stream";
				return false;
			}
			if(stream.front == c){
				value = c;
				stream.popFront();
				return true;
			}
			else {
				err.location = stream.location;
				err.reason = msg;
				return false;
			}
		}
	}

	/// Single element token.
	auto tk(alias c)(){ return Tk!c(); }

	/// Single element token, skipping any whitespace at front.
	auto stk(alias c)()
	if(is(typeof(c) : ElementType!Stream) && is(typeof(c) : dchar)){
		import pry.combinators;
		return tk!c.skipWs;
	}

	struct Range(alias low, alias high)
	if(is(typeof(low): ElementType!Stream) && is(typeof(high) : ElementType!Stream)){
		static immutable msg = "expected in a range of " ~ to!string(low) ~ ".." ~ to!string(high);
			
		bool parse(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err){
			if(stream.empty) {
				err.location = stream.location;
				err.reason = "unexpected end of stream";
				return false;
			}
			auto v = stream.front;
			if(v >= low && v <= high) {
				value = v;
				stream.popFront();
				return true;
			}
			else {
				err.location = stream.location;
				err.reason = msg;
				return false;
			}
		}
	}
	
	// In a range of elements.
	auto range(alias low, alias high)(){ return Range!(low, high)(); }

	interface DynamicParser(V) {
		bool parse(ref Stream stream, ref V value, ref Stream.Error err);
	}

	// Use LINE & FILE to provide unique types of dynamic.
	auto dynamic(V, size_t line=__LINE__, string file=__FILE__)(){
		static class Dynamic : DynamicParser!V {
			DynamicParser!V wrapped;
		final:
			void opAssign(P)(P parser)
			if(isParser!P && !is(P : Dynamic)){
				wrapped = wrap(parser);
			}
	
			bool parse(ref Stream stream, ref V value, ref Stream.Error err){
				assert(wrapped, "Use of empty dynamic parser");
				return wrapped.parse(stream, value, err); 
			}
		}	
		return new Dynamic();
	}

	auto wrap(Parser)(Parser parser)
	if(isParser!Parser){
		alias V = ParserValue!Parser;
		static class Wrapped: DynamicParser!V {
			Parser p;
			
			this(Parser p){
				this.p = p;
			}

			bool parse(ref Stream stream, ref V value, ref Stream.Error err){
				return p.parse(stream, value, err); 
			}
		}
		return new Wrapped(parser);
	}

	struct Set(alias set) {
		import std.uni, std.conv;
		static if(set.byInterval.length <= 6) {
			mixin(set.toSourceCode("test"));
		}
		else {
			static assert(0);
		}

		static immutable string msg = "expected one of " ~ to!string(set);

		bool parse(ref Stream stream, ref dchar value, ref Stream.Error err){
			if(stream.empty){
				err.location = stream.location;
				err.reason = "unexpected end of stream";
			}
			immutable c = stream.front;
			if(test(c)){
				value = stream.front;
				stream.popFront();
				return true;
			}
			err.location = stream.location;
			err.reason = msg;
			return false;
		}
	}

	auto set(alias s)(){
		import std.uni;
		static assert(isCodepointSet!(typeof(s)), "set only works with std.uni.CodepointSet");
		return Set!s();
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto parser = dynamic!dchar;
		parser = tk!'a';
		S s = S("a");
		dchar c;
		S.Error err;
		assert(parser.parse(s, c, err));
		assert(c == 'a');
		assert(s.empty);
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto s = " a".stream;
		auto p = stk!'a';
		dchar c;
		S.Error err;
		assert(p.parse(s, c, err));
		assert(c == 'a');
		assert(s.empty);

		auto s2 = "a".stream;
		assert(p.parse(s2, c, err));
		assert(c == 'a');
		assert(s2.empty);
	}
}

unittest {
	import std.uni;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto p = set!(CodepointSet('A', 'Z'+1, 'a', 'z'+1));
		auto s = "aZ0".stream;
		dchar c;
		S.Error err;
		assert(p.parse(s, c, err));
		assert(c == 'a');
		assert(p.parse(s, c, err));
		assert(c == 'Z');
		assert(!p.parse(s, c, err));
		assert(s.front == '0');
		auto p2 = set!(unicode.L);
	}
}
