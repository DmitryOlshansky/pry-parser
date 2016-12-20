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
enum isParser(alias p) = is(typeof((){
	alias Stream = ParserStream!p;
	alias Value = ParserValue!p;
	Stream stream;
	Value value;
	Stream.Error error;
	bool r = p(stream, value, error);
}));

/// Extract value type of a given Parser.
alias ParserValue(alias parser) = Parameters!(parser)[1];

/// Extract stream type of a given parser.
alias ParserStream(alias parser) = Parameters!(parser)[0];
