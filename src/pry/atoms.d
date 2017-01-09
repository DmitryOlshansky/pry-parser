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

	auto dynamic(V)(){
		static class Dynamic : DynamicParser!V {
			DynamicParser!V wrapped;
		final:
			void opAssign(P)(P parser)
			if(isParser!P && !is(P : Dynamic)){
				wrapped = wrap(parser);
			}
	
			bool parse(ref Stream stream, ref V value, ref Stream.Error err){
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
}

unittest
{
	alias S = SimpleStream!string;
	with(parsers!S)
	{
		auto parser = dynamic!dchar;
		parser = tk!'a';
		S s = S("a");
		dchar c;
		S.Error e;
		assert(parser.parse(s, c, e));
		assert(c == 'a');
		assert(s.empty);
	}
}
