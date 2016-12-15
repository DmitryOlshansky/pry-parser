module pry.combinators;

import pry.traits;
import std.meta;

private auto repImpl(bool collect, size_t minTimes, P)(P parser)
if(isParser!P){
	alias Stream = ParserStream!P;
	static struct Parser {
		private P p;
		static if(collect)
			ParserValue!P[] value;
		else
			Stream.Range value;

		bool parse(ref Stream stream) {
			auto start = stream.mark;
			for(size_t i = 0; i<minTimes; i++) {
				if(!p.parse(stream)){
					stream.restore(start);
					return false;
				}
				static if(collect) value ~= p.value;
			}
			while(p.parse(stream)){
				static if(collect) value ~= p.value;
			}
			static if(!collect)
				value = stream.slice(start);
			return true;
		}

		Stream.Error error() {
			return p.error;
		}
	}
	return Parser();
}


/// Apply parser for minTimes times or more and return consumed range.
auto rep(size_t minTimes=1, P)(P parser)
if(isParser!P){
	return repImpl!(false, minTimes, P)(parser);
}

/// Apply parser for minTimes times or more and return array of results.
auto array(size_t minTimes=1, P)(P parser)
if(isParser!P){
	return repImpl!(true, minTimes, P)(parser);
}

unittest
{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S)
	{
		auto p = rep!2(tk!'a');
		auto s = S("aaac");
		assert(p.parse(s));
		assert(p.value == "aaa");
		
		s = S("a");
		assert(!p.parse(s));
		
		auto p2 = range!('a', 'b').array;
		s = S("aba");
		assert(p2.parse(s));
		assert(p2.value == "aba"d);
		assert(s.empty);
	}
}

/// Apply a mapping function to the value of parser.
auto map(alias f, P)(P parser)
if(isParser!P) {
	alias Stream = ParserStream!P;
	static struct Parser {
		private P p;
		typeof(f(p.value)) value;

		bool parse(ref Stream stream) {
			if(p.parse(stream)){
				value = f(p.value);
				return true;
			}
			else
				return false;
		}

		Stream.Error error() {
			return p.error;
		}
	}
	return Parser(parser);
}

///
unittest {
	import std.conv;
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto digits = range!('0', '9').rep.map!(x=>x.to!int);
		S s = S("90");
		assert(digits.parse(s));
		assert(digits.value == 90);
		s = S("a");
		assert(!digits.parse(s));
		assert(digits.error.context == 0);
	}
}

/// Apply multiple parsers one after another as a sequence.
auto seq(P...)(P parsers)
if(allSatisfy!(isParser, P)) {
	import std.typecons;
	alias Stream = ParserStream!(P[0]);
	alias Values = staticMap!(ParserValue, P);
	static struct Parser {
		private P parsers;
		Tuple!Values value;
		private size_t errorIndex;

		bool parse(ref Stream stream) {
			auto save = stream.mark;
			foreach(i, ref p; parsers) {
				if(!p.parse(stream)){
					errorIndex = i;
					stream.restore(save);
					return false;
				}
				value[i] = p.value;
			}
			return true;
		}

		Stream.Error error() {
			foreach(i, ref p; parsers) {
				if(i == errorIndex){
					return p.error;
				}
			}
			assert(false);
		}
	}
	return Parser(parsers);
}

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.typecons;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto elements = seq(tk!'a', range!('a', 'd'), tk!'c');
		S s = S("abc");
		assert(elements.parse(s));
		assert(s.empty);
		assert(elements.value == tuple('a', 'b', 'c'));
		s = S("axc");
		assert(!elements.parse(s));
		assert(s.front == 'a');
		assert(elements.error.context == 1);
	}
}

/// Try each of provided parsers until one succeeds.
auto any(P...)(P parsers)
if(allSatisfy!(isParser, P)) {
	import std.variant;
	alias Stream = ParserStream!(P[0]);
	alias Values = NoDuplicates!(staticMap!(ParserValue, P));
	static struct Parser {
		private P parsers;
		static if(Values.length == 1)
			Values[0] value;
		else
			Algebraic!Values value;

		bool parse(ref Stream stream) {
			foreach(i, p; parsers) {
				if(p.parse(stream)){
					value = p.value;
					return true;
				}
			}
			return false;
		}

		Stream.Error error() {
			return parsers[$-1].error();
		}
	}
	return Parser(parsers);
}

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.conv;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto digits = range!('0', '9').rep.map!(x => x.to!int);
		auto parser = any(tk!'a', digits);
		S s = "10a";
		assert(parser.parse(s));
		assert(parser.value == 10);
		assert(parser.parse(s));
		assert(parser.value == cast(dchar)'a');
		assert(s.empty);
	}
}

unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto e = dynamic!int;
		e = any(seq(tk!'0', e).map!(x => 1), tk!'1'.map!(x => 0));
		S s = S("0001");
		assert(e.parse(s));
	}
}