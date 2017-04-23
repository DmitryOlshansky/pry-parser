module pry.grammar.graph;

import pry;
import pry.grammar.ast;
import pry.grammar.parser;
import std.exception;

private:

class CollectDependencies(T) : Visitor {
  T[string] defs;
  T[string] deps;

  this(T[string] definitions) {
    defs = definitions;
  }

  override void visit(Grammar grammar) {
    foreach(def; grammar.defs)
      def.accept(this);
  }

  override void visit(Definition def) {
    def.ast.accept(this);
  }

  override void visit(Sequence seq) {
    foreach(ast; seq.seq) {
      ast.accept(this);
    }
  }

  override void visit(Alternative alt) {
    foreach(ast; alt.alt) {
      ast.accept(this);
    }
  }

  override void visit(Map map) { 
    map.ast.accept(this);
  }

  override void visit(NegativeLookahead neg) {
    neg.ast.accept(this);
  }

  override void visit(PositiveLookahead pos) {
    pos.ast.accept(this);
  }

  override void visit(SimpleSequence seq){}

  override void visit(CharClass charClass){}

  override void visit(Literal literal){}

  override void visit(Reference reference) {
    enforce(reference.name in defs,
      "Undefined non-terminal '"~reference.name~"'");
    deps[reference.name] = defs[reference.name];
  }
}

T[string] collectDependenices(T)(Definition target, T[string] defs) {
  auto visitor = new CollectDependencies!T(defs);
  target.accept(visitor);
  return visitor.deps;
}

class Node {
  Definition def;
  Node[] deps;  // directed dependencies of this node
  int priority; // order of definition, bigger is higher priority

  this(Definition def){
    this.def = def;
  }
}

struct Graph {
  Node[] nodes;

  this(Definition[] definitions) {
    import std.algorithm, std.range, std.array, std.typecons;
    nodes = definitions.map!(x => new Node(x)).array;
    Node[string] name2node;
    foreach(i, d; definitions) {
      name2node[d.name] = nodes[i];
    }
    foreach(n; nodes) {
      n.deps = collectDependenices(n.def, name2node).values();
    }
  }

  void dfs(Node n) {
    import std.algorithm, std.array;
    Node[] path;
    void _dfs(Node start, int priority){
      if(start.priority < priority) start.priority = priority;
      path ~= start;
      for(size_t i = 0; i < start.deps.length;){
        Node next = start.deps[i];
        if(!find!"a is b"(path, next).empty) {
          // Found a cycle.
          next.def.dynamic = true;
          // Break it.
          start.deps = remove!(SwapStrategy.unstable)(start.deps, i);
          continue;
        }
        _dfs(next, priority+1);
        i++;
      }
      path = path[0..$-1];
    }
    _dfs(n, 1);
  }

  Node[] topologicalSort() {
    import std.algorithm;
    foreach(n; nodes){
      if(n.priority == 0) dfs(n);
    }
    sort!((a,b) => a.priority > b.priority, SwapStrategy.stable)(nodes);
    return nodes;
  }
}

unittest {
  import std.algorithm, std.array;
  Definition[] defs = `
  G:
    A < 'a' B?;
    B < 'b' C?;
    C < 'c' A?;
  `.parse(pegParser).defs;
  auto nodes = Graph(defs).topologicalSort();
  assert(nodes.map!(x => x.def.name).equal(["C", "B", "A"]));
  assert(!nodes[0].def.dynamic);
  assert(!nodes[1].def.dynamic);
  assert(nodes[2].def.dynamic);
}

unittest {
  import std.algorithm, std.array;
  Definition[] defs = `
  G:
    D < 'd' D?;
    C < 'c' D;
    B < 'b' C;
    A < 'a' B;
    E < '12';
  `.parse(pegParser).defs;
  auto nodes = Graph(defs).topologicalSort();
  assert(nodes.map!(x => x.def.name).equal(["D", "C", "B", "A", "E"]));
  assert(nodes[0].def.dynamic);
  assert(!nodes[1].def.dynamic);
  assert(!nodes[2].def.dynamic);
  assert(!nodes[3].def.dynamic);
  assert(!nodes[4].def.dynamic);
}


class CodeGen : Visitor {
  import std.algorithm : filter, map;
  import std.array, std.format;
  import std.range: iota;
  Appender!string app;
  string streamType;
  bool skipWhite, useRep;

  this(string streamType, bool skipWhite) {
    app = appender!string();
    this.streamType = streamType;
    this.skipWhite = skipWhite;
  }

  void outputModifier(Modifier mod) {
    if(mod.min == 1 && mod.max == 1)
      return;
    if(mod.min == 0 && mod.max == 1){
      formattedWrite(app, ".optional");
      return;
    }
    if(useRep) 
      formattedWrite(app, ".rep!(%d, %d)", mod.min, mod.max);
    else
      formattedWrite(app, ".array!(%d, %d)", mod.min, mod.max);
  }

  override void visit(Grammar g) {
    auto nodes = Graph(g.defs).topologicalSort();
    formattedWrite(app,
      "immutable %s = (){\nimport pry, std.uni;\nwith(parsers!(%s)){\n",
      g.name, streamType);
    foreach(n; nodes) {
      if(!n.def.dynamic) continue;
      if(n.def.type == "")
        throw new Exception("PEG needs type for circular definition: "~n.def.name);
      formattedWrite(app, "auto %s = dynamic!%s;\n", n.def.name, n.def.type);
    }
    foreach(n; nodes) {
      n.def.accept(this);
    }
    formattedWrite(app, "return %s;\n", g.defs[0].name);
    formattedWrite(app, "}\n}();");
  }

  override void visit(Definition def) {
    formattedWrite(app, "%s %s = ", def.dynamic ? "" : "auto", def.name);
    def.ast.accept(this);
    formattedWrite(app, ";\n");
  }

  override void visit(Sequence seq) {
    if(seq.seq.length != 1) formattedWrite(app, "seq(");
    foreach(i, ast; seq.seq) {
      if(i != 0) formattedWrite(app, ",");
      ast.accept(this);
    }
    if(seq.seq.length != 1) formattedWrite(app, ")");
    // Map away ignored pieces in the grammar
    auto liveIdx = iota(0, seq.seq.length)
      .filter!(i => !seq.seq[i].ignored).array;
    if(liveIdx.length != seq.seq.length) {
      bool noTuple = liveIdx.length == 1;
      if(noTuple) {
        formattedWrite(app, ".map!(x => x[%s])", liveIdx[0]);
      }
      else {
        formattedWrite(app, ".map!(x => tuple(");
        foreach(i; liveIdx) {
          formattedWrite(app, "%sx[%s]", i != 0 ? "," : "", i);
        }
        formattedWrite(app, "))");
      }
    }
  }

  override void visit(Alternative alt) {
    if(alt.alt.length != 1) formattedWrite(app, "any(");
    foreach(i, ast; alt.alt) {
      if(i != 0) formattedWrite(app, ",");
      ast.accept(this);
    }
    if(alt.alt.length != 1) formattedWrite(app, ")");
    outputModifier(alt.mod);
  }

  override void visit(Map map) { 
    map.ast.accept(this);
    formattedWrite(app, ".map!((it)%s)", map.code);
    outputModifier(map.mod);
  }

  override void visit(NegativeLookahead neg) {
    neg.ast.accept(this);
    formattedWrite(app, ".negLookahead");
  }

  override void visit(PositiveLookahead pos) {
    pos.ast.accept(this);
    formattedWrite(app, ".lookahead");
  }

  override void visit(SimpleSequence seq){
    if(seq.seq.length != 1)
      formattedWrite(app, "seq(");
    bool save = skipWhite;
    skipWhite = false;
    useRep = true;
    foreach(i, ast; seq.seq) {
      if(i != 0) formattedWrite(app, ",");
      ast.accept(this);
    }
    useRep = false;
    skipWhite = save;
    if(seq.seq.length != 1)
      formattedWrite(app, ").slice");
    if(skipWhite)
      formattedWrite(app, ".skipWs");
  }

  override void visit(CharClass charClass) {
    formattedWrite(app, "set!(CodepointSet(");
    bool first = true;
    foreach(ival; charClass.set.byInterval) {
      formattedWrite(app, "%s%s,%s", first ? "" : ",", ival.a, ival.b);
      if(first) first = false;
    }
    formattedWrite(app, "))");
    outputModifier(charClass.mod);
    if(skipWhite) formattedWrite(app, ".skipWs");
  }

  override void visit(Literal literal) {
    if(literal.lit.length == 1 
      || (literal.lit.length == 2  && literal.lit[0] == '\\')) {
      formattedWrite(app, "tk!'%s'", literal.lit);
    }
    else {
      formattedWrite(app, `literal!"%s"`, literal.lit);
    }
    outputModifier(literal.mod);
    if(skipWhite) formattedWrite(app, ".skipWs");
  }

  override void visit(Reference reference) {
    formattedWrite(app, "%s", reference.name);
    outputModifier(reference.mod);
  }
}

public enum PegOption: uint {
  skipWhite = 1
};

public string grammar(S=string)(string peg, PegOption options=PegOption.skipWhite) {
  Grammar g;
  try {
    g = peg.parse(pegParser);
  }
  catch(ParseFailure!(SimpleStream!string) failure) {
    throw new Exception("PEG grammar error "
      ~ peg[0..failure.err.location] ~ " --HERE--> "
      ~ peg[failure.err.location..$]);
  }
  auto generator = new CodeGen(S.stringof, (options & PegOption.skipWhite) != 0);
  generator.visit(g);
  return generator.app.data();
}

unittest {
  enum string expr = `
  calc:
    expr : int < 
      (term '+' expr) { return it[0] + it[2]; } 
      / (term '-' expr) { return it[0] - it[2]; }
      / term ;
    term : int <
      (primary '*' term) { return it[0] * it[2]; }
      / (primary '/' term) { return it[0] / it[2]; }
      / primary ;
    primary < 
      [0-9]+ { return to!int(it); } 
      / ^'(' expr ^')';
  `;
  import std.stdio, std.conv, pry;
  writeln(grammar(expr, PegOption.skipWhite));
  mixin(grammar(expr, PegOption.skipWhite));
  assert(" ( 2 + 4) * 2".parse(calc) == 12);
}

