module json;

import pry;

import std.stdio, std.conv, std.uni;

alias S = SimpleStream!string;

static immutable allowedChars =
		unicode.Cc.add('"', '"'+1).add('\\', '\\'+1).inverted;
static immutable hex = CodepointSet('0', '9'+1, 'a', 'f'+1, 'A', 'F'+1);

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
				seq(range!('1', '9'), digit.rep!0).optional
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

version(unittest){}
else
void main(){
	
}
