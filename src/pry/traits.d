module pry.traits;

import std.range.primitives, std.traits;

/// Test if Stream is some stream.
enum isStream(Stream) = is(typeof((){
	Stream stream;
	auto error = Stream.Error(stream.context, "description");
	auto c = error.context;
	string s = error.reason;
	size_t k = stream.mark;
	stream.restore(k);
	Stream.Range slice = stream.slice(k);
})) && isInputRange!Stream;

/// Test if Parser is some parser.
enum isParser(Parser) = is(typeof((){
	Parser parser;
	alias Stream = ParserStream!Parser;
	Stream stream;
	auto v = parser.value;
	bool r = parser.parse(stream);
	Stream.Error s = parser.error;
}));

/// Extract value type of a given Parser.
alias ParserValue(Parser) = typeof(Parser.init.value);

/// Extract stream type of a given Parser.
alias ParserStream(Parser) = Parameters!(Parser.parse)[0];
