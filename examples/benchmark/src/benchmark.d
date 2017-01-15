
import std.datetime, std.range, std.random, std.stdio, std.conv, std.exception;
import pry.stream;

alias S = SimpleStream!string;

auto makeParser(){
	import pry;

	with(parsers!S) {
		auto expr = dynamic!int;
		auto primary = any(
			range!('0', '9').rep.map!(x => x.to!int),
			seq(tk!'(', expr, tk!')').map!(x => x[1])
		);
		auto term = dynamic!int;
		term = any(
			seq(primary, tk!'*', term).map!(x => x[0] * x[2]),
			seq(primary, tk!'/', term).map!(x => x[0] / x[2]),
			primary
		);
		expr = any(
			seq(term, tk!'+', expr).map!(x => x[0] + x[2]),
			seq(term, tk!'-', expr).map!(x => x[0] - x[2]),
			term
		);
		return expr;
	}
}

auto rng = Xorshift(42);

string generateSample(int depth){
	if(depth == 0){
		int len = uniform(1, 6, rng);
		return generate!(() => cast(char)(uniform(0, 10, rng) + '0'))
				.take(len).array.assumeUnique;
	}
	int dice = uniform(0, 5, rng);
	switch(dice){
	case 0:
		return "(" ~ generateSample(depth-1) ~ ")";
	case 1:
		return generateSample(depth-1) ~ "+" ~ generateSample(depth-1);
	case 2:
		return generateSample(depth-1) ~ "-" ~ generateSample(depth-1);
	case 3:
		return generateSample(depth-1) ~ "*" ~ generateSample(depth-1);
	case 4:
		return generateSample(depth-1);
	default:
		assert(false);
	}
}

void main(){
	auto sample = generateSample(20);
	auto parser = makeParser();
	int value;
	StopWatch sw;
	sw.start();
	foreach(i; 0..100){
		S s = S(sample);
		int v;
		S.Error err;
		parser.parse(s, v, err);
		value += v;
	}
	sw.stop();
	writefln("Value: %d\nTime: %s ms", value, sw.peek().msecs);
}