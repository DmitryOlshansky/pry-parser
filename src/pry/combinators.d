module pry.combinators;

import pry.traits;
import std.meta;

struct RepImpl(bool collect, size_t minTimes, Parser){
	alias Stream = ParserStream!Parser;
	static if(collect)
		alias Value = ParserValue!Parser[];
	else
		alias Value = Stream.Range;
	Parser parser;

	bool parse(ref Stream stream, ref Value value, ref Stream.Error err) {
		auto start = stream.mark;
		ParserValue!Parser tmp;
		for(size_t i = 0; i<minTimes; i++) {
			if(!parser.parse(stream, tmp, err)){
				stream.restore(start);
				return false;
			}
			static if(collect) value ~= tmp;
		}
		while(parser.parse(stream, tmp, err)){
			static if(collect) value ~= tmp;
		}
		static if(!collect)
			value = stream.slice(start);
		return true;
	}
}


/// Apply parser for minTimes times or more and return consumed range.
auto rep(size_t minTimes=1, Parser)(Parser parser)
if(isParser!Parser){
	return RepImpl!(false, minTimes, Parser)(parser);
}

/// Apply parser for minTimes times or more and return array of results.
auto array(size_t minTimes=1, Parser)(Parser parser)
if(isParser!Parser){
	return RepImpl!(true, minTimes, Parser)(parser);
}

unittest
{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S)
	{
		auto p = tk!'a'.rep!2;
		string r;
		S.Error err;
		auto s = S("aaac");
		assert(p.parse(s, r, err));
		assert(r == "aaa");
		
		s = S("a");
		assert(!p.parse(s, r, err));
		assert(err.location == 1);
		assert(err.reason == "unexpected end of stream");
		
		auto p2 = range!('a', 'b').array;
		dchar[] r2;
		s = S("aba");
		assert(p2.parse(s, r2, err));
		assert(r2 == "aba"d);
		assert(s.empty);
	}
}


struct Map(Parser, alias f) {
	alias Stream = ParserStream!Parser;
	alias Value = typeof(f(ParserValue!Parser.init));
	alias mapper = f;
	Parser parser;
	bool parse(ref Stream stream, ref Value value, ref Stream.Error err){
		ParserValue!Parser tmp;
		if(parser.parse(stream, tmp, err)){
			value = f(tmp);
			return true;
		}
		else
			return false;
	}
}

/// Apply a mapping function to the value of parser.
template map(alias f)
{
	auto map(Parser)(Parser parser)
	if(isParser!Parser){
		return Map!(Parser, f)(parser);
	}
}

///
unittest {
	import std.conv;
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto digits = range!('0', '9').rep.map!(x=>x.to!int);
		alias f = digits.mapper;
		S s = S("90");
		int r;
		S.Error err;
		assert(digits.parse(s, r, err));
		assert(r == 90);
		s = S("a");
		assert(!digits.parse(s, r, err));
		assert(err.location == 0);
	}
}



struct Seq(P...){
	import std.typecons;
	alias Stream = ParserStream!(P[0]);
	alias Values = staticMap!(ParserValue, P);
	P parsers;
	
	bool parse(ref Stream stream, ref Tuple!Values value, ref Stream.Error err) {
		auto save = stream.mark;
		foreach(i, ref p; parsers) {
			if(!p.parse(stream, value[i], err)){
				stream.restore(save);
				return false;
			}
		}
		return true;
	}
}

/// Apply multiple parsers one after another as a sequence.
auto seq(P...)(P parsers)
if(allSatisfy!(isParser, P)){
	return Seq!P(parsers);
}

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.typecons;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto elements = seq(tk!'a', range!('a', 'd'), tk!'c');
		S s = S("abc");
		Tuple!(dchar, dchar, dchar) val;
		S.Error err;
		assert(elements.parse(s, val, err));
		assert(s.empty);
		assert(val == tuple('a', 'b', 'c'));
		s = S("axc");
		assert(!elements.parse(s, val, err));
		assert(s.front == 'a');
		assert(err.location == 1);
	}
}


struct Any(P...){
	import std.variant;
	alias Stream = ParserStream!(P[0]);
	alias Values = NoDuplicates!(staticMap!(ParserValue, P));

	static if(Values.length == 1)
		alias Value = Values[0];
	else
		alias Value = Algebraic!Values;
	P parsers;

	bool parse(ref Stream stream, ref Value value, ref Stream.Error err) {
		Stream.Error current;
		foreach(i, ref p; parsers) {
			ParserValue!(P[i]) tmp;
			static if(i == 0){
				if(p.parse(stream, tmp, err)){
					value = tmp;
					return true;
				}
			}
			else {
				if(p.parse(stream, tmp, current)){
					value = tmp;
					return true;
				}
				// pick the deeper error
				if(err.location < current.location){
					err = current;
				}
			}
		}
		return false;
	}
}

/// Try each of provided parsers until one succeeds.
auto any(P...)(P parsers)
if(allSatisfy!(isParser, P)){
	return Any!P(parsers);
}


auto fn(Parser)(Parser parser){
	alias Stream = ParserStream!Parser;
	alias Value = ParserValue!Parser;
	Stream stream;
	Value value;
	Parser parser;
	Stream.Error error;
	bool r = parser.parse(stream, value, error);
}

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.conv, std.variant;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto digits = range!('0', '9').rep.map!(x => x.to!int);
		static assert(isParser!(typeof(digits)));
		auto parser = any(tk!'a', digits);
		S s = "10a";
		S.Error err;
		Algebraic!(dchar, int) value;
		assert(parser.parse(s, value, err));
		assert(value == 10);
		assert(parser.parse(s, value, err));
		assert(value == cast(dchar)'a');
		assert(s.empty);
	}
}

unittest {
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto e = dynamic!int;
		e = any(
			seq(tk!'0', e).map!(x => 1),
			tk!'1'.map!(x => 0)
		);
		S.Error err;
		int val;
		S s = S("0001");
		assert(e.parse(s, val, err));
	}
}

unittest {
	import pry.atoms, pry.stream;
	import std.typecons;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto p = any(
			seq(tk!'0', tk!'0'),
			seq(tk!'1', tk!'1')
		);
		S s = "01".stream;
		Tuple!(dchar, dchar) value;
		S.Error err;
		assert(!p.parse(s, value, err));
		assert(err.location == 1);
	}
}

