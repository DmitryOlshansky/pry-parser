module pry.grammar.printer;

import pry.grammar.ast;
import std.stdio, std.conv, std.format;

class PrettyPrinter : Visitor {
	string modToString(Modifier mod) {
		if(mod.min == 1 && mod.max == 1)
			return "";
		if(mod.min == 0 && mod.max == 1)
			return "?";
		if(mod.min == 0 && mod.max == uint.max)
			return "*";
		if(mod.min == 1 && mod.max == uint.max)
			return "+";
		return format("{%d,%d}", mod.min, mod.max);
	}

	override void visit(Grammar grammar) {
		writefln("%s:", grammar.name);
		foreach(d; grammar.defs)
			d.accept(this);
	}

	override void visit(Definition def) {
		string type = def.type == "" ? "" : format(": %s", def.type);
		writef("%s %s <- ", def.name, type);
		def.ast.accept(this);
		writeln();
	}

	override void visit(Sequence seq) {
		foreach(ast; seq.seq) ast.accept(this);
	}

	override void visit(Alternative alt) {
		writef("%s(", alt.ignored ? ":" : "");
		foreach(i, ast; alt.alt) {
			if(i != 0) write(" / ");
			ast.accept(this);
		}
		writef(")%s", modToString(alt.mod));
	}

	override void visit(Map map) {
		map.ast.accept(this);
		writef("%s", map.code);
	}

	override void visit(NegativeLookahead neg) {
		write("!");
		neg.ast.accept(this);
	}

	override void visit(PositiveLookahead pos) {
		write("&");
		pos.ast.accept(this);
	}

	override void visit(SimpleSequence seq) {
		writef("%s", seq.ignored ? ":" : "");
		foreach(ast; seq.seq)
			ast.accept(this);
	}

	override void visit(CharClass charClass) {
		writef("%s%s%s", charClass.ignored ? ":" : "",
			to!string(charClass.set), modToString(charClass.mod));
	}

	override void visit(Literal literal) {
		writef("%s'%s'%s", literal.ignored ?  "^" : "", literal.lit,
			modToString(literal.mod));
	}

	override void visit(Reference reference) {
		writef("%s%s%s", reference.ignored ?  ":" : "", reference.name,
			modToString(reference.mod));
	}

	override void visit(Combinator combinator) {
		writef("$%s(", combinator.combinator);
		foreach(i, arg; combinator.args) {
			if(i != 0) write(",");
			arg.accept(this);
		}
		write(")");
	}
}

void prettyPrint(Grammar grammar) {
	auto pp = new PrettyPrinter();
	grammar.accept(pp);
}
