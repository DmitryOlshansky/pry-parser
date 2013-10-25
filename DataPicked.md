DataPicked grammar itself in EBNF-like notation.
ASCII whitespace is ignored, Unicode one is disallowed.
Plus the comments are C-style, line-only 
```
// This is a comment
```
May start at any place in the text.

```
DataPickedSchema : Declaration* RootDeclaration Declaration*
```

```
RootDeclaration : 'root' '=' DataExpr ';'
```

```
Declaration : Name '=' DataExpr ';'
```

A single declaration defines a single layer of hierachical structure. A layer that contains no primary aliases (see later) is assumed to be annonymous and its entities are aliased directly to the name (if possible, otherwise it's a compile error).

```
DataExpr : DataSequence
         : DataAlternative
```
`DataExpr` defines an entity in a data format. 

```
DataSequence : DataPiece (',' DataPiece)*
```
`DataSequence` by default is going to be a tuple of whatever pieces that constituite it. A single element tuple is aliased directly to its single member.

```
DataAlternative : DataPiece ('|' DataPiece )+
```
`DataAlternative` is a taged union of its contents. Sequence is prefered so a single `DataPiece` is a `DataSequence`.

```
DataPiece : DataAtom
          : DataAtom '*'
          : DataAtom '?'
          : DataAtom '{' Expr (',' Expr )? '}'
```
Depending on `DataAtom` the piece becomes a single item, a collection, a Nullable, or a fixed size collection. Pieces form level of a hierarchical entity that a complete declaration defines.

```
DataAtom : EntityExpr AliasExpr 
         : '(' DataExpr ')' AliasExpr
```

`DataAtom` may be a parenthesized `DataExpr`. In such a case alias supplied ties the whole of DataExpr as a single entity - this is second way to introduce another layer of hierarchy. If parentisized expression is not aliased then it's assumed to be "merged" with the same layer it was on.


```
AliasExpr : '!'? Name? (':' AliasAtom*)?
```
Name is considered primary alias - by this name it's accessible in the application later on. Note that using the same name in one layer of hierarchy means "accumulate". Thus 2 atoms aliased to the same name in one layer of declaration would yeild a collection of 2 elements.

The '!' bit in AliasExpr indicates to alias the entity expresion to the name but to not keep the data. Without '!' it's automatically made a field in the declared element iff EntityExp has external entries or contains primary aliases.

With name this essentially makes it a deduced vairable just like the ones `AliasAtom` defines.

```
AliasAtom : Expr '->' Name
```
`AliasAtom` introduces deduced variable that is not stored directly but may be used elsewhere, for instance to define size of a `DataPiece`. In future these could be used to test semantic predicates on data.

```
EntityExpr : Name 
           : BytePattern
           : StringPattern
```

```
StringPattern : '"' CharClass+ '"'
```

```
CharClass : [^\\\[\]]
          : '\' [\\\[\]\"]
          : '[' '^'? RangeExpr+ ']'
```

```
BytePattern : ByteClass+
```

```
ByteClass : Number
          : '[' '^'? RangeExpr+ ']'
```

```
RangeExpr : Number
          : Number '-' Number
```

```
Expr : `the whole tree of D-expressions except assignments`
```

```
Name : [a-zA-Z_][a-zA-Z_0-9]*

Number : [0-9]+
       : '0' 'x' [0-9A-Fa-f]+
```
