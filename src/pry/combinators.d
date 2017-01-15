module pry.combinators;

import pry.traits;
import std.meta;

private:

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
public auto rep(size_t minTimes=1, Parser)(Parser parser)
if(isParser!Parser){
	return RepImpl!(false, minTimes, Parser)(parser);
}

/// Apply parser for minTimes times or more and return array of results.
public auto array(size_t minTimes=1, Parser)(Parser parser)
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
public template map(alias f)
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
public auto seq(P...)(P parsers)
if(allSatisfy!(isParser, P)){
	static if(P.length == 0)
		return Nothing();
	else
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

struct Nothing{}

struct TList(T...){}

template commonPrefixLength(T1, T2){
	static if(is(T1 == TList!U1, U1...)){
		static if(is(T2 == TList!U2, U2...)){
			static if(U1.length == 0 || U2.length == 0){
				enum commonPrefixLength = 0;
			}
			else static if(is(U1[0] == U2[0])){
				enum commonPrefixLength = 1 +
					commonPrefixLength!(TList!(U1[1..$]), TList!(U2[1..$]));
			}
			else {
				enum commonPrefixLength = 0;
			}
		}
	}
}

auto commonPrefix(P1, P2)(P1 p1, P2 p2){
	static if(is(P1 : Seq!U1, U1...)){
		enum isSeq = true;
		alias T1 = TList!U1;
	}
	else{
		enum isSeq = false;
		alias T1 = TList!P1;
	}
	static if(is(P2 : Seq!U2, U2...)){
		alias T2 = TList!U2;
	}
	else{
		alias T2 = TList!P2;
	}
	enum len = commonPrefixLength!(T1,T2);
	static if(isSeq)
		return seq(p1.parsers[0..len]);
	else static if(len)
		return seq(p1);
	else
		return Nothing();
}

auto commonPrefix(P...)(P p)
if(P.length > 2){
	return commonPrefix(commonPrefix(p[0], p[1]), p[2..$]);
}

unittest {
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S){
		auto p1 = seq(tk!'a', tk!'b');
		auto p2 = seq(tk!'a');
		auto p3 = tk!'a';
		auto p4 = seq(tk!'b', tk!'a');
		assert(commonPrefix(p1, p2) == seq(tk!'a'));
		assert(commonPrefix(p2, p3) == seq(tk!'a'));
		assert(commonPrefix(p3, p3) == seq(tk!'a'));
		assert(commonPrefix(p3, p1) == seq(tk!'a'));
		assert(commonPrefix(p1, p4) == Nothing());
		assert(commonPrefix(p3, p4) == Nothing());
		assert(commonPrefix(p4, p3) == Nothing());
		assert(commonPrefix(p1, p2, p3) == seq(tk!'a'));
		assert(commonPrefix(p1, p2, p3, p4) == Nothing());
	}
}

auto suffix(P1, P2)(P1 prefix, P2 parser)
if(isParser!P1 && isParser!P2){
	static if(is(P1 : Seq!U1, U1...)){
		alias T1 = TList!U1;
	}
	else{
		alias T1 = TList!P1;
	}
	static if(is(P2 : Seq!U2, U2...)){
		enum isSeq = true;
		alias T2 = TList!U2;
	}
	else{
		enum isSeq = false;
		alias T2 = TList!P2;
	}
	enum len = commonPrefixLength!(T1,T2);
	static if(is(T1 : TList!U, U...)){
		static assert(len == U.length);
	}
	static if(isSeq){
		return seq(parser.parsers[len..$]);
	}
	else{
		static if(len > 0)
			return Nothing();
		else
			return seq(parser);
	}
}

unittest{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S){
		auto p1 = seq(tk!'a', tk!'b');
		auto p2 = seq(tk!'a');
		auto p3 = tk!'a';
		auto p4 = seq(tk!'b', tk!'a');
		assert(suffix(p2, p1) == seq(tk!'b'));
		assert(suffix(p3, p1) == seq(tk!'b'));
		assert(suffix(p2, p3) == Nothing());
	}
}

template Unmap(P){
	static if(is(P : Map!(U, f), alias f, U)){
		alias Unmap = U;
	}
	else {
		alias Unmap = P;
	}
}

auto unmap(P)(P parser){
	static if(is(P : Map!(U, f), alias f, U)){
		return parser.parser;
	}
	else {
		return parser;
	}
}

unittest{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S){
		auto x = tk!'a'.map!(x => 1);
		assert(unmap(x) == tk!'a');
		assert(unmap(unmap(x)) == tk!'a');
	}
}

struct Any(P...){
	import std.variant, std.typecons;
	alias Stream = ParserStream!(P[0]);
	alias Values = NoDuplicates!(staticMap!(ParserValue, P));

	static if(Values.length == 1)
		alias Value = Values[0];
	else
		alias Value = Algebraic!Values;
	
	P parsers;
	alias Prefix = typeof(extractPrefix());
	Prefix prefix;

	template mapper(size_t i){
		static if(is(P[i] == Map!(U, f), U, alias f))
			alias mapper = P[i].mapper;
		else
			alias mapper = x => x;
	}

	this(P parsers){
		this.parsers = parsers;
		prefix = extractPrefix();
	}

	auto extractPrefix(){
		staticMap!(Unmap, P) unmapped = void;
		foreach(i, ref p; parsers){
			unmapped[i] = unmap(p);
		}
		return commonPrefix(unmapped);
	}

	auto combine(T1, T2)(T1 prefixValue, T2 suffixValue){
		static if(is(T1 == Nothing)){
			return suffixValue;
		}
		else{
			return tuple(prefixValue.expand, suffixValue.expand);
		}
	}

	bool parse(ref Stream stream, ref Value value, ref Stream.Error err) {
		static if(is(Prefix == Nothing)){
			Nothing prefixValue;
		}
		else{
			ParserValue!Prefix prefixValue;
			if(!prefix.parse(stream, prefixValue, err)){
				return false;
			}
		}
		Stream.Error current;
		bool ret = false;
		foreach(i, ref p; parsers) {
			static if(is(Prefix == Nothing))
				auto sp = unmap(p);
			else
				auto sp = suffix(prefix, unmap(p));
			alias Suffix = typeof(sp);
			static if(is(Suffix == Nothing)){
				value = mapper!i(prefixValue.expand);
				ret = true;
				goto L_end;
			}
			else {
				ParserValue!Suffix suffixValue;
				static if(i == 0){
					if(sp.parse(stream, suffixValue, err)){
						value = mapper!i(combine(prefixValue, suffixValue));
						return true;
					}
				}
				else {
					if(sp.parse(stream, suffixValue, current)){
						value = mapper!i(combine(prefixValue, suffixValue));
						return true;
					}
					// pick the deeper error
					if(err.location < current.location){
						err = current;
					}
				}
			}
		}
L_end:
		return ret;
	}
}

/// Try each of provided parsers until one succeeds.
public auto any(P...)(P parsers)
if(allSatisfy!(isParser, P)){
	return Any!P(parsers);
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
