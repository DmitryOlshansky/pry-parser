Pry
==========

Pry is an experimental flexible data extraction library

On the surface Pry is a PEG-inspired parser generator with special bits being:

* optional compile-time code-generation
* support for "parsing" of TLV (type, length, value) records common to binary formats
* easily pluggable scanner/tokenizer with flexiblity to e.g. use it only for one particular token and built-in PEG syntax for others
* "AST" is data-centeric, that is everything is extracted as a DAG consisting of tagged unions, tuples, vectors, nullable types


### Grammar

Pry grammar itself in EBNF-like notation.
ASCII whitespace is ignored, Unicode one is disallowed.
Plus the comments are C-style, line-only 
```
// This is a comment
```
May start at any place in the text.

```
PrySchema : Declaration*
```

```
Declaration : Rule | ImportDeclaration | OptionDeclaration
```

```
Rule : Name ':' DataExpr ';'
```
A single production rule that  defines a single layer of hierachical structure, in a PEG-style definition.

```
ImportDeclaration: 'import' Id
```
Import declaration adds a D module import to the resulting parser,
thereby allowing to use names from that file for low-level scanners.

```
OptionDeclaration: Id '=' Name | Number
```
A declaration that sets various global options such as 'filter' for
pre-processing bytes/characters and 'type' to define a unit of processing - character or ubyte or anything else. Note that byte ranges 
and character ranges work for char and ubyte types respectively.

```
DataExpr : DataSeq ('|' DataSeq)*

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
DataAtom : (Id '=')? EntityExprt 
         : (Id '=')?'(' DataExpr ')'
```

`DataAtom` may be a parenthesized `DataExpr`. In such a case alias supplied ties the whole of DataExpr as a single entity - this is second way to introduce another layer of hierarchy.

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
