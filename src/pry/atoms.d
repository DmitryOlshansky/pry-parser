module pry.atoms;

import pry.stream, pry.traits;
import std.conv, std.range.primitives;

template parsers(Stream)
{
	// Single element token.
	auto tk(alias c)()
	if(is(typeof(c) : ElementType!Stream)) {
		static struct Parser {
			ElementType!Stream value = c;

			static immutable msg = "expected '" ~ to!string(c) ~ "'";

			bool parse(ref Stream stream) {
				if(stream.empty) return false;
				if(stream.front == value){
					stream.popFront();
					return true;
				}
				else
					return false;
			}

			auto error(ref Stream stream){
				return Stream.Error(stream.context, msg);
			}
		}
		return Parser();
	}

	// In a range of elements.
	auto range(alias low, alias high)()
	if(is(typeof(low): ElementType!Stream) && is(typeof(high) : ElementType!Stream)){
		static struct Parser {
			ElementType!Stream value;

			static immutable msg = "expected in a range of " ~ to!string(low) ~ ".." ~ to!string(high);
			
			bool parse(ref Stream stream) {
				if(stream.empty) return false;
				auto v = stream.front;
				if(v >= low && v < high) {
					stream.popFront();
					value = v;
					return true;
				}
				else
					return false;
			}
		}
	}

	interface Dynamic(V) {
		@property ref V value();
		bool parse(ref Stream stream);
		Stream.Error error(ref Stream stream);
	}

	auto dynamic(Parser)(Parser parser)
	if(isParser!Parser){
		alias V = ParserValue!Parser;
		static class Wrapped : Dynamic!V {
			Parser p;
			
			override ref V value(){ return p.value; }

			override bool parse(ref Stream stream){ return p.parse(stream); }

			override Stream.Error error(ref Stream stream){ return p.error(stream); }

			this(Parser p){
				this.p = p;
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
		Dynamic!dchar parser;
		pragma(msg, ParserStream!(Dynamic!char));
		parser = dynamic(tk!'a');
		S s = S("a");
		assert(parser.parse(s));
		assert(parser.value == 'a');
		assert(s.empty);
	}
}