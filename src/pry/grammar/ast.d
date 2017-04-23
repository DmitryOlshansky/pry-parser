module pry.grammar.ast;

import std.uni;

struct Modifier {
	uint min = 1;
	uint max = 1;
}

class Ast {
	Modifier mod;
	bool ignored;

	abstract void accept(Visitor visitor);
}

abstract class Visitor {
	void visit(Grammar grammar);
	void visit(Definition def);
	void visit(Sequence seq);
	void visit(Alternative alt);
	void visit(Map map);
	void visit(NegativeLookahead neg);
	void visit(PositiveLookahead pos);
	void visit(SimpleSequence seq);
	void visit(CharClass charClass);
	void visit(Literal literal);
	void visit(Reference refernce);
	void visit(Combinator combinator);
}

mixin template Visitable() {
	override void accept(Visitor visitor){
		visitor.visit(this);
	}
}

class Grammar : Ast {
	string name;
	Definition[] defs;

	this(string name, Definition[] defs) {
		this.name = name;
		this.defs = defs;
	}
	mixin Visitable;
}

class Definition : Ast {
	string name;
	string type;
	Ast ast;
  	bool dynamic; // if should be codegened as dynamic!T

	this(string name, string type, Ast ast)
	{
		this.name = name;
		this.type = type;
		this.ast = ast;
	}
	mixin Visitable;
}

class Sequence : Ast {
	Ast[] seq;

	this(Ast[] sequence){ seq = sequence; }
	mixin Visitable;
}

class Alternative : Ast {
	Sequence[] alt;

	this(Sequence[] alternatives){ alt = alternatives; }
	mixin Visitable;
}

class Map : Ast {
	Ast ast;
	string code;

	this(Ast subject, string mapper){
		ast = subject;
		code = mapper;
	}
	mixin Visitable;
}

class NegativeLookahead : Ast {
	Ast ast;

	this(Ast ast){ this.ast = ast; }
	mixin Visitable;
}

class PositiveLookahead : Ast {
	Ast ast;

	this(Ast ast){ this.ast = ast; }
	mixin Visitable;
}

// A sequence that consits only of char classes and literals
// constructed implicitly 
class SimpleSequence : Ast {
	Ast[] seq;

	this(Ast[] sequence){ seq = sequence; }
	mixin Visitable;
}

class Reference : Ast {
	string name;

	this(string name){ this.name = name; }
	mixin Visitable;
}

class CharClass : Ast {
	CodepointSet set;

	this(CodepointSet charset){ set = charset; }
	mixin Visitable;
}

class Literal : Ast {
	string lit;

	this(string literal){ lit = literal; }
	mixin Visitable;
}

class Combinator : Ast {
	string combinator;
	Ast[] args;

	this(string combinator, Ast[] args) {
		this.combinator = combinator;
		this.args = args;
	}
	mixin Visitable;
}

