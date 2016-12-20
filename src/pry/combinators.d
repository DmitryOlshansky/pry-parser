module pry.combinators;

import pry.traits;
import std.meta;

template repImpl(bool collect, size_t minTimes, alias parser){
	alias Stream = ParserStream!parser;
	static if(collect)
		alias Value = ParserValue!parser[];
	else
		alias Value = Stream.Range;

	bool repImpl(ref Stream stream, ref Value value, ref Stream.Error err) {
		auto start = stream.mark;
		ParserValue!parser tmp;
		for(size_t i = 0; i<minTimes; i++) {
			if(!parser(stream, tmp, err)){
				stream.restore(start);
				return false;
			}
			static if(collect) value ~= tmp;
		}
		while(parser(stream, tmp, err)){
			static if(collect) value ~= tmp;
		}
		static if(!collect)
			value = stream.slice(start);
		return true;
	}
}


/// Apply parser for minTimes times or more and return consumed range.
alias rep(alias parser, size_t minTimes=1) = repImpl!(false, minTimes, parser);

/// Apply parser for minTimes times or more and return array of results.
alias array(alias parser, size_t minTimes=1) = repImpl!(true, minTimes, parser);

unittest
{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S)
	{
		alias p = rep!(tk!'a', 2);
		string r;
		S.Error err;
		auto s = S("aaac");
		assert(p(s, r, err));
		assert(r == "aaa");
		
		s = S("a");
		assert(!p(s, r, err));
		assert(err.location == 1);
		assert(err.reason == "unexpected end of stream");
		
		alias p2 = array!(range!('a', 'b'));
		dchar[] r2;
		s = S("aba");
		assert(p2(s, r2, err));
		assert(r2 == "aba"d);
		assert(s.empty);
	}
}

/// Apply a mapping function to the value of parser.
template map(alias parser, alias f)
/*if(isParser!parser)*/ {
	alias Stream = ParserStream!parser;
	alias Value = typeof(f(ParserValue!parser.init));
	bool map(ref Stream stream, ref Value value, ref Stream.Error err){
		ParserValue!parser tmp;
		if(parser(stream, tmp, err)){
			value = f(tmp);
			return true;
		}
		else
			return false;
	}
}

///
unittest {
	import std.conv;
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S) {
		alias digits = map!(rep!(range!('0', '9')), x=>x.to!int);
		S s = S("90");
		int r;
		S.Error err;
		assert(digits(s, r, err));
		assert(r == 90);
		s = S("a");
		assert(!digits(s, r, err));
		assert(err.location == 0);
	}
}


/// Apply multiple parsers one after another as a sequence.
template seq(P...)
/*if(allSatisfy!(isParser, P)) */{
	import std.typecons;
	alias Stream = ParserStream!(P[0]);
	alias Values = staticMap!(ParserValue, P);
	
	bool seq(ref Stream stream, ref Tuple!Values value, ref Stream.Error err) {
		auto save = stream.mark;
		foreach(i, p; P) {
			if(!p(stream, value[i], err)){
				stream.restore(save);
				return false;
			}
		}
		return true;
	}
}

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.typecons;
	alias S = SimpleStream!string;
	with(parsers!S) {
		alias elements = seq!(tk!'a', range!('a', 'd'), tk!'c');
		S s = S("abc");
		Tuple!(dchar, dchar, dchar) val;
		S.Error err;
		assert(elements(s, val, err));
		assert(s.empty);
		assert(val == tuple('a', 'b', 'c'));
		s = S("axc");
		assert(!elements(s, val, err));
		assert(s.front == 'a');
		assert(err.location == 1);
	}
}


/// Try each of provided parsers until one succeeds.
template any(P...)
/*if(allSatisfy!(isParser, P)) */{
	import std.variant;
	alias Stream = ParserStream!(P[0]);
	alias Values = NoDuplicates!(staticMap!(ParserValue, P));

	static if(Values.length == 1)
		alias Value = Values[0];
	else
		alias Value = Algebraic!Values;
	Stream.Error error;

	bool any(ref Stream stream, ref Value value, ref Stream.Error err) {
		Stream.Error current;
		foreach(i, p; P) {
			ParserValue!p tmp;
			static if(i == 0){
				if(p(stream, tmp, err)){
					value = tmp;
					return true;
				}
			}
			else {
				if(p(stream, tmp, current)){
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

///
unittest {
	import pry.atoms, pry.stream;
	import std.range.primitives, std.conv, std.variant;
	alias S = SimpleStream!string;
	with(parsers!S) {
		alias digits = map!(rep!(range!('0', '9')), x => x.to!int);
		alias parser = any!(tk!'a', digits);
		S s = "10a";
		S.Error err;
		Algebraic!(dchar, int) value;
		assert(parser(s, value, err));
		assert(value == 10);
		assert(parser(s, value, err));
		assert(value == cast(dchar)'a');
		assert(s.empty);
	}
}

unittest {
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto e = dynamic!int;
		e = &any!(
			map!(seq!(tk!'0', e), x => 1),
			map!(tk!'1', x => 0)
		);
		S.Error err;
		int val;
		S s = S("0001");
		assert(e(s, val, err));
	}
}

unittest {
	import pry.atoms, pry.stream;
	import std.typecons;
	alias S = SimpleStream!string;
	with(parsers!S) {
		alias p = any!(
			seq!(tk!'0', tk!'0'),
			seq!(tk!'1', tk!'1')
		);
		S s = "01".stream;
		Tuple!(dchar, dchar) value;
		S.Error err;
		assert(!p(s, value, err));
		assert(err.location == 1);
	}
}
