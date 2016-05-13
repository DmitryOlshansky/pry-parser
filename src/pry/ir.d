module pry.ir;

import std.exception;
import pry.ast, pry.misc;


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

class Codegen{

}

//the building block of IR
class Node {
    Target target; // == null, no data extraction in this node
    void generate(Codegen codegen);
}

class ByteNode : Node{
    ubyte  value;
}

class ByteClassNode : Node {
    BitMask!8 value;
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


class Scope {
    Target[string] table;
    Scope parent;

    this(Target[string] tab, Scope p)
    {
        table = tab;
        parent = p;
    }

    Target opIndex(string idx)
    {
        auto p = idx in table;
        if(p)
            return *p;
        if(parent)
            return parent[idx];
        enforce(false, "Variable "~idx~" not found!");
        assert(false);
    }

    void opIndex(Target target, string idx)
    {
        table[idx] = target;
    }

    Scope push()
    {
        return new Scope(null, this);
    }

    Scope pop()
    {
        return parent;
    }
}

Node toIR(Ast[string] decls)
{
    return null;
}