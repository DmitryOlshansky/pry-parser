DataPicked grammar itself in EBNF-like notation.

```
DataPickedSchema : Declaration* RootDeclaration Declaration*
```

```
RootDeclaration : 'root' '=' DataExpr ';'
```

```
Declaration : id '=' DataExpr ';'
```

```
DataExpr : DataSequence
         : DataAlternative
```
`DataExpr` defines a entity in a data format. 

```
DataSequence : DataPiece (',' DataPiece)*
```
`DataSequence` by default is going to be a tuple of whatever pieces that constituite it. A single element tuple is aliased directly to its single member.

```
DataAlternative : DataPiece '|' DataPiece ('|' DataPiece )*
```
`DataAlternative` is a taged union of its contents. Sequence is prefered so a single `DataPiece` is a `DataSequence`.

```
DataPiece : DataAtom
          : DataAtom '*'
          : DataAtom '?'
          : DataAtom '{' Expr (',' Expr )? '}'
```
Depending on `DataAtom` the piece becomes a single item, a collection, a Nullable, or a fixed size collection. Pieces form level of hierarchical entity that a complete declaration defines.

```
DataAtom : EntityExpr AliasExpr 
         : '(' DataExpr ')' AliasExpr
```

```
AliasExpr : ('!'? Name)? (':' AliasAtom*)?
```

The '!' bit in AliasExpr indicates to alias the entity expresion to the name but to not keep the data. Without '!' it's automatically made a field in the declared element.
This essentially makes it a deduced vairable just like the ones `AliasAtom` defines.

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
          : '\' [\\\[\]]
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
