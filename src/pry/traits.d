module pry.traits;

import std.range.primitives, std.traits;

/// Test if Stream is some stream.
enum isStream(Stream) = is(typeof((){
	Stream stream;
	auto error = Stream.Error(stream.location, "description");
	auto c = error.location;
	string s = error.reason;
	size_t k = stream.mark;
	stream.restore(k);
	Stream.Range slice = stream.slice(k);
})) && isInputRange!Stream;

/// Test if p is some parser.
enum isParser(Parser) = is(typeof((ref Parser parser){
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
