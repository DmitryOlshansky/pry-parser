module json;

import pry;

import std.stdio, std.conv, std.uni, std.variant, std.typecons,
	std.json, std.datetime;;
import file = std.file;
import stdx.data.json;

import taggedalgebraic;

struct JSValue {
	union Base {
		typeof(null) null_;
		bool boolean;
		double num;
		string str;
		JSValue[] array;
		JSValue[string] object;
	}
	alias Payload = TaggedAlgebraic!Base;
	Payload payload;
	alias payload this;
	this(T)(T value){ payload = Payload(value); }
}

alias S = SimpleStream!string;

enum allowedChars =
		unicode.Cc.add('"', '"'+1).add('\\', '\\'+1).inverted;
enum hex = CodepointSet('0', '9'+1, 'a', 'f'+1, 'A', 'F'+1);

auto jsString(){
	with(parsers!S) {
		auto unescaped_string = set!allowedChars.rep!0;
		auto escaped_string = any(
			set!allowedChars,
			seq(tk!'\\', any(
				tk!'"',
				tk!'\\',
				tk!'/',
				tk!'b'.map!(_ => '\b'),
				tk!'f'.map!(_ => '\f'),
				tk!'n'.map!(_ => '\n'),
				tk!'r'.map!(_ => '\r'),
				tk!'t'.map!(_ => '\t'),
				seq(tk!'u', set!hex.rep!(4,4)).map!(x => cast(dchar)to!int(x[1], 16))
			)).map!(x => x[0])
		).utfString!(char, 0);
		auto full = seq(tk!'"', any(unescaped_string, escaped_string), tk!'"');
		return full.skipWs.map!(x => x[1]);
	}
}

unittest {
	assert(`""`.parse(jsString) == "");
	assert(`"abc"`.parse(jsString) == "abc");
}

auto jsNumber(){
	with(parsers!S) {
		auto digit = range!('0','9');
		return seq(
			tk!'-'.optional,
			any(
				tk!'0',
				seq(range!('1', '9'), digit.rep!0)
			), // got to skip whitespace in front of tokens
			// optional fraction
			seq(
				tk!'.',
				digit.rep!1
			).optional,
			// optional exponent
			seq(
				any(tk!'e', tk!'E'),
				any(tk!'+', tk!'-').optional,
				digit.rep!1
			).optional
		).slice.skipWs.map!(x => to!double(x));
	}
}

unittest{
	assert("0".parse(jsNumber) == 0);
	assert(" -1e2".parse(jsNumber) == -100);
	assert("3.1415".parse(jsNumber) == 3.1415);
}

auto jsonParser(){
	with(parsers!S) {
		auto jsValue = dynamic!JSValue;
		auto pair = seq(jsString, stk!':', jsValue).map!(x => tuple(x[0], x[2]));
		auto jsObject = seq(
			tk!'{',
			seq(
				pair, seq(stk!',', pair).map!(x => x[1]).aa!0
			).optional.map!((x){
				if(x.isNull) return null;
				auto head = x[0];
				auto aa = x[1];
				aa[head[0]] = head[1];
				return aa;
			}),
			stk!'}'
		).skipWs.map!(x => x[1]);
		auto jsArray = seq(
			tk!'[',
			delimited(jsValue, stk!','),
			stk!']'
		).skipWs.map!(x => x[1]);
		jsValue = any(
			jsString.map!(x => JSValue(x)),
			jsNumber.map!(x => JSValue(x)),
			jsObject.map!(x => JSValue(x)),
			jsArray.map!(x => JSValue(x)),
			literal!"true".map!(x => JSValue(true)),
			literal!"false".map!(x => JSValue(false)),
			literal!"null".map!(x => JSValue(null))
		);
		return jsValue;
	}
}

unittest {
	auto v = `{ "a": 12, "b": [1,2,3 ], "c" : true, "null" : null }`.parse(jsonParser);
	assert(v["a"] == 12);
	assert(v["b"] == [ 1, 2, 3 ]);
	assert(v["c"] == true);
	assert(v["null"] == null);
}

void parseStd(string data){
	parseJSON(data);
}

void parsePry(string data){
	data.parse(jsonParser);
}

void parseDataJson(string data){
	toJSONValue(data);
}

void main(string[] argv){
	void usage(){
		writeln("Usage:\n./json <file>");
	}
	if(argv.length != 2) return usage();
	const iterations = 1_000;
	string data = cast(string)file.read(argv[1]);
	auto results = benchmark!(
		() => parseStd(data),
		() => parsePry(data),
		() => parseDataJson(data)
	)(iterations);
	writefln("std.json: %s us\npry: %s us\nstdx.data.json %s us\n",
		results[0].usecs / cast(double)iterations,
		results[1].usecs / cast(double)iterations,
		results[2].usecs / cast(double)iterations);
}
