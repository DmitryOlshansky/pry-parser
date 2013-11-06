module dpick.ir;

import dpick.ast, dpick.misc;

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

class Node //the building block of IR
{
    Target target; // == null, no data extraction in this node
}

class TupleNode : Node
{
    Node[] seq;
}

class UnionNode : Node
{
    Node[] alt;
}

class RepNode : Node
{
    Node element;
    Expr low, high;
}

class ComputeNode : Node
{
    Expr expr;
}


class Scope{
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

struct Context{
    Ast[string] decls;
    Scope _scope; //chain of variable scopes (as named layers go)
    
    void push(){ _scope = _scope.push(); }
    void pop(){ _scope = _scope.pop(); }
}

Node toIR(Ast[string] decls)
{
    return null;
}