module pry.grammar.parser;

import pry;
import pry.grammar.ast, pry.grammar.printer;
import std.conv, std.exception, std.uni;

alias Stream = SimpleStream!string;

struct SkipWs2(P) {
	private P parser;
	alias Stream = ParserStream!P;
	alias Value = ParserValue!P;

	bool parse(ref Stream s, ref Value v, ref Stream.Error err){
		while(!s.empty) {
			immutable c = s.front;
			if(c != ' ') break;
			s.popFront();
		}
		return parser.parse(s, v, err);
	}
}

/// Skip whitespace at front then apply the `parser`.
public auto skipWs2(P)(P parser)
if(isParser!P){
	return SkipWs2!P(parser);
}

auto modifier() {
	with(parsers!Stream) {
		auto digits = range!('0', '9').rep.skipWs2.map!(x => to!int(x));
		return any(
			tk!'*'.map!(x => Modifier(0, uint.max)),
			tk!'+'.map!(x => Modifier(1, uint.max)),
			tk!'?'.map!(x => Modifier(0, 1)),
			seq(tk!'{', digits, seq(stk!',', digits).optional, stk!'}')
				.map!(x => Modifier(x[1], x[2].isNull ? x[1] : x[2][1]))
		);
	}
}

unittest {
	auto m = modifier();
	assert("{ 2, 4 }".parse(m) == Modifier(2, 4));
	assert("{ 1 }".parse(m) == Modifier(1, 1));
	assert("*".parse(m) == Modifier(0, uint.max));
	assert("+".parse(m) == Modifier(1, uint.max));
	assert("?".parse(m) == Modifier(0, 1));
}

struct CharClassParser {
	bool parse(ref Stream stream, ref CodepointSet set, ref Stream.Error err) {
		auto m = stream.mark();
		try {
			set = unicode.parseSet(stream);
			return true;
		}
		catch(Exception e){
			stream.restore(m);
			err.reason = e.msg;
			err.location = stream.location;
			return false;
		}
	}
}

auto charClass(){
	with(parsers!Stream) {
		return CharClassParser().map!(x => cast(Ast)new CharClass(x));
	}
}

unittest {
	auto cs = charClass();
	assert((cast(CharClass)"[0-9]".parse(cs)).set == CodepointSet('0', '9'+1));
	auto s = "[0-9".stream;
	Stream.Error err;
	Ast set;
	assert(!cs.parse(s, set, err));
	assert(s.front == '[');
}

auto literalAtom(){
	enum notQuote = CodepointSet('\'', '\''+1, '\\', '\\'+1).inverted;
	enum hex = CodepointSet('0', '9'+1, 'a', 'f'+1, 'A', 'F'+1);
	with(parsers!Stream) {
		auto p = seq(
			tk!'\'',
			any(
				set!notQuote, 
				seq(tk!'\\', any(
					tk!'"',
					tk!'\'',
					tk!'\\',
					tk!'/',
					tk!'b'.map!(_ => cast(dchar)'\b'),
					tk!'f'.map!(_ => cast(dchar)'\f'),
					tk!'n'.map!(_ => cast(dchar)'\n'),
					tk!'r'.map!(_ => cast(dchar)'\r'),
					tk!'t'.map!(_ => cast(dchar)'\t'),
					seq(tk!'x', set!hex.rep!(2,2)).map!(x => cast(dchar)to!int(x[1], 16)),
					seq(tk!'u', set!hex.rep!(4,4)).map!(x => cast(dchar)to!int(x[1], 16))
				)).map!(x => x[1])
			).utfString!(char, 0),
			tk!'\''
		).map!(x => cast(Ast)new Literal(x[1]));
		return p;
	}
}

unittest {
	assert((cast(Literal)`'abc\''`.parse(literalAtom)).lit == `abc'`);
	assert((cast(Literal)`'\u2340\x90'`.parse(literalAtom)).lit == "\u2340\u0090");
}

auto identifier(){
	enum start = CodepointSet('a', 'z'+1, 'A', 'Z'+1, '_', '_'+1);
	enum end = CodepointSet('a', 'z'+1, 'A', 'Z'+1, '0', '9'+1, '_', '_'+1);
	with(parsers!Stream) {
		return seq(set!start, set!end.rep!0).slice;
	}
}

unittest {
	assert("a".parse(identifier) == "a");
	assert("_90".parse(identifier) == "_90");
}

auto pegParser() {
	with(parsers!Stream) {
		auto alternative = dynamic!Ast;
		auto simpleAtom = any(
			charClass,
			literalAtom
		);
		auto atomBase = seq(
			tk!'^'.optional, 
			any(
				seq(simpleAtom, modifier.optional).map!((x){
					auto ast = x[0];
					if(!x[1].isNull) ast.mod = x[1];
					return ast; 
				}).skipWs2.array.map!(x => cast(Ast)new SimpleSequence(x)),
				seq(identifier, modifier.optional).map!((x){
					auto ast = cast(Ast)new Reference(x[0]);
					if(!x[1].isNull) ast.mod = x[1];
					return ast;
				}).skipWs2,
				seq(tk!'(', alternative, stk!')', modifier.optional).map!((x){
					auto ast = x[1];
					if(!x[3].isNull) ast.mod = x[3];
					return ast;
				}).skipWs2
			)
		).map!((x){ if(!x[0].isNull) x[1].ignored = true; return x[1]; });
		auto atom = any(
			seq(tk!'!', atomBase).map!(x => cast(Ast)new NegativeLookahead(x[1])),
			seq(tk!'&', atomBase).map!(x => cast(Ast)new PositiveLookahead(x[1])),
			atomBase
		);
		auto sequence = atom.array.map!(x => new Sequence(x));
		alternative = delimited(sequence, stk!'/').skipWs2
			.map!(x => cast(Ast)new Alternative(x));
		auto definitions = seq(
			identifier.skipWs2, seq(stk!':', identifier.skipWs2).optional, 
			stk!'>', alternative
		).map!(x => new Definition(x[0], x[1].isNull ? "" : x[1][1], x[3]))
			.array.skipWs;
		return definitions;
	}
}

unittest {
	import std.stdio;
	string s = `
	abc : Type > [0-9]+ ^'a' / 'b' abc
	def > ^( '456' abc ){2} '90' !([a-c][d-f])[a-z]+ 
	`;
	try {
		prettyPrint(s.parse(pegParser));
	}
	catch(ParseFailure!Stream ex){
		writeln(s[ex.err.location .. $]);
	}
}
