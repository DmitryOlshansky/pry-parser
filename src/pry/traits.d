module pry.traits;

import std.range.primitives, std.traits;

/// Test if Stream is some stream.
enum isStream(Stream) = is(typeof((Stream stream){
	auto error = Stream.Error(stream.location, "description");
	auto c = error.location;
	string s = error.reason;
	size_t k = stream.mark;
	stream.restore(k);
	Stream.Range slice = stream.slice(k);
})) && isInputRange!Stream;

/// Test if p is some parser.
enum isParser(Parser) = is(typeof((ref Parser parser){
	static assert(isCallable!(Parser.parse));
	alias Stream = ParserStream!Parser;
	alias Value = ParserValue!Parser;
	Stream stream;
	Value value;
	Stream.Error error;
	bool r = parser.parse(stream, value, error);
}));

/// Extract value type of a given Parser.
alias ParserValue(Parser) = Parameters!(Parser.parse)[1];

/// Extract stream type of a given parser.
alias ParserStream(Parser) = Parameters!(Parser.parse)[0];

class ParseFailure(S) : Exception 
if(isStream!S){
	S.Error err;

	this(S.Error err, string file = __FILE__, size_t line = __LINE__,
			Throwable next = null){
		super(err.reason, file, line, next);
		this.err = err;
	}

	override string toString(){
		import std.format;
		return format("Parse failure at %s: %s", err.location, err.reason);
	}
}

/// Convenience wrapper for Parser interface - parse a string, throw on failure.
auto parse(Parser, S)(S str, Parser parser)
if(isParser!Parser && isSomeString!S){
	import pry.stream;
	alias Stream = SimpleStream!S;
	auto stream = str.stream;
	ParserValue!Parser value;
	Stream.Error err;
	if(!parser.parse(stream, value, err)){
		throw new ParseFailure!Stream(err);
	}
	return value;
}

unittest{
	import pry.atoms, pry.stream;
	import std.exception;
	alias S = SimpleStream!string;
	with(parsers!S){
		auto p = tk!'a';
		assert("a".parse(p) == 'a');
		assertThrown("".parse(p));
	}
}
