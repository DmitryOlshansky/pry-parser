module pry.grammar.graph;

import pry;
import pry.grammar.ast;
import pry.grammar.parser;
import std.exception;

class CollectDependencies(T) : Visitor {
  T[string] defs;
  T[string] deps;

  this(T[string] definitions) {
    defs = definitions;
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
  bool dynamic; // if should be codegened as dynamic!T

  this(Definition def){
    this.def = def;
  }
}

struct Graph {
  Node[] nodes;

  this(Definition[] definitions) {
    import std.algorithm, std.range, std.array, std.typecons;
    nodes = definitions.map!(x => new Node(x)).array;
    auto name2node = definitions.zip(iota(0, definitions.length))
                    .map!(x => tuple(x[0].name, nodes[x[1]])).assocArray;
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
          next.dynamic = true;
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
    A > 'a' B?
    B > 'b' C?
    C > 'c' A?
  `.parse(pegParser);
  auto nodes = Graph(defs).topologicalSort();
  assert(nodes.map!(x => x.def.name).equal(["C", "B", "A"]));
  assert(!nodes[0].dynamic);
  assert(!nodes[1].dynamic);
  assert(nodes[2].dynamic);
}

unittest {
  import std.algorithm, std.array;
  Definition[] defs = `
    D > 'd' D?
    C > 'c' D
    B > 'b' C
    A > 'a' B
    E > '12'
  `.parse(pegParser);
  auto nodes = Graph(defs).topologicalSort();
  assert(nodes.map!(x => x.def.name).equal(["D", "C", "B", "A", "E"]));
  assert(nodes[0].dynamic);
  assert(!nodes[1].dynamic);
  assert(!nodes[2].dynamic);
  assert(!nodes[3].dynamic);
  assert(!nodes[4].dynamic);
}

