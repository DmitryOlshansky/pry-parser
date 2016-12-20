module calc;

import pry;

import std.stdio, std.conv;

alias S = SimpleStream!string;

auto makeParser(){
	with(parsers!S) {
		auto expr = dynamic!int;
		alias primary = any!(
			map!(rep!(range!('0', '9')), x => x.to!int),
			map!(seq!(tk!'(', expr, tk!')'), x => x[1])
		);
		auto term = dynamic!int;
		term = &any!(
			map!(seq!(primary, tk!'*', term), x => x[0] * x[2]),
			map!(seq!(primary, tk!'/', term), x => x[0] / x[2]),
			primary
		);
		expr = &any!(
			map!(seq!(term, tk!'+', expr), x => x[0] + x[2]),
			map!(seq!(term, tk!'-', expr), x => x[0] - x[2]),
			term
		);
		return expr;
	}
}

void main(){
	auto parser = makeParser();
	while(!stdin.eof){
		string s = readln();
		int value;
		S.Error err;
		auto stream = S(s[0..$-1]); // chomp '\n'
		bool success = parser(stream, value, err);
		if(success && stream.empty){
			writeln("=", value);
		}
		else if(success && !stream.empty){
			writeln("Failed to parse, the leftover is : ", s[stream.location..$-1]);
		} else {
			writeln("Error at ", err.location, " ", err.reason);
		}
	}
}
