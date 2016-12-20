module pry.atoms;

import pry.stream, pry.traits;
import std.conv, std.range.primitives;

template parsers(Stream)
{
	// Single element token.
	auto tk(alias c)
	(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err)
	if(is(typeof(c) : ElementType!Stream)) {
		static immutable msg = "expected '" ~ to!string(c)~"'";
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

	// In a range of elements.
	auto range(alias low, alias high)
	(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err)
	if(is(typeof(low): ElementType!Stream) && is(typeof(high) : ElementType!Stream)){
		static immutable msg = "expected in a range of " ~ to!string(low) ~ ".." ~ to!string(high);
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

	interface DynamicParser(V) {
		bool opCall(ref Stream stream, ref V value, ref Stream.Error err);
	}

	auto dynamic(V)(){
		static class Dynamic : DynamicParser!V {
			DynamicParser!V wrapped;
		final:
			void opAssign(P)(P parser)
			if(isParser!parser && !is(P : Dynamic)){
				wrapped = wrap(parser);
			}
	
			bool opCall(ref Stream stream, ref V value, ref Stream.Error err){
				return wrapped(stream, value, err); 
			}
		}	
		return new Dynamic();
	}

	auto wrap(Parser)(Parser parser)
	if(isParser!parser){
		alias V = ParserValue!parser;
		static class Wrapped: DynamicParser!V {
			Parser p;
			
			this(Parser p){
				this.p = p;
			}

			bool opCall(ref Stream stream, ref V value, ref Stream.Error err){
				return p(stream, value, err); 
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
		static assert(isParser!(tk!'a'));
		auto parser = dynamic!dchar;
		parser = &tk!'a';
		S s = S("a");
		dchar c;
		S.Error e;
		assert(parser(s, c, e));
		assert(c == 'a');
		assert(s.empty);
	}
}
