Pry
==========

On the surface Pry is a PEG-inspired parser combinator library.
The focus of development is pragmatic qualities such as achieving performance on par with handwritten parsers,
at the expense of idealistic goals to e.g. support the most features of the PEG grammars.

Special bits are:
- completely generic input via "Stream", a thin wrapper on top of D ranges
- compile-time optimized building blocks for things like 'one of a set values', 'given value'
- support for parsing TLV (type, length, value) style records commonly found in binary formats
- optional grammar-driver parser generator on top of combinators framework

