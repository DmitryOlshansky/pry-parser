module dpick.ir;

import std.exception;
import dpick.ast, dpick.buffer, dpick.misc;


class Target
{
    string type;
    string id;
}

// 1 exactly
class Value(T) : Target {

}

// 0 or more
class Collection(T) : Target {

}

// 0 or 1
class Optional(T) : Target {

}

//the building block of IR
class Node {
    Target target; // == null, no data extraction in this node
}

class ByteNode : Node{
    ubyte  value;
}

class ByteClassNode : Node {
    BitMask!8 value;
}

enum isExtractFunctor(alias Fn) = __traits(compiles, (BufferConcept buf){
    auto x = Fn(buf);
    cast(void)x;
});

alias ExtractorType(alias Fn) = typeof((){ BufferConcept buf; return Fn(buf); }());

//define own type 
struct UserNode(alias extractFn)
        if(isExtractFunctor!extractFn) {
    alias extract = extractFn;
    alias Type = ExtractorType!extractFn;
}


static assert(isExtractFunctor!(buf=>buf.front));
static assert(is(UserNode!(buf=>buf.front).Type == ubyte));

class CustomNode(UserSpec) : Node {
    UserSpec.Type value;
    alias extract = UserSpec.extract;
}

class TupleNode : Node {
    Node[] seq;
}

class UnionNode : Node {
    Node[] alt;
}

class RepNode : Node {
    Node element;
    Expr low, high;
}

class ComputeNode : Node {
    Expr expr;
}

// ref-link to a "sub-program" of Nodes 
// a subsection in the hierarchy of Data
class LayerNode : Node {
    string id;
    Node layer; // the whole layer as Tuple/Union or some other node
}

class Scope {
    Target[string] table;
    Scope* parent;

    Target opIndex(string idx)
    {
        auto p = idx in table;
        if(p)
            return *p;
        if(parent)
            return (*parent)[idx];
        enforce(false, "Variable "~idx~" not found!");
    }

    void opIndex(Target target, string idx)
    {
        table[idx] = target;
    }

    Scope push()
    {
        return new Scope([], this);
    }

    Scope pop()
    {
        return parent;
    }
}

struct Context {
    Ast[string] decls;
    Scope _scope; //chain of variable scopes (as named layers go)
    
    void push(){ _scope = _scope.push(); }
    void pop(){ _scope = _scope.pop(); }

    Node toIR(Ast ast)
    {
        return ast.match!(
            
        );
    }
}

Node toIR(Ast[string] decls)
{
    return null;
}