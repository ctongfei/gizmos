## Poly-collection

Poly-collection is a Scala collection framework that aims to provide an alternative to the standard
 Scala collection framework. It provides a large range of collection classes, and differs from
 the standard collection framework in several ways:
 
  - Full support for trees and graphs, with all kinds of functional operators defined on them.
  
  - Lazy by default: Higher-order transformational functions are nearly always executed in a non-strict manner:
   functions like `map`, `filter`, `flatMap` are always evaluated lazily. This allows chain application like 
   `xs.map(f).filter(g).reduce(h)` to be much faster than the standard library.
  
  - Various algorithms provided out of the box: binary search; disjoint sets; A* search; ...

  - Concretely based on basic algebraic concepts through [poly-algebra](https://github.com/ctongfei/poly-algebra) by extensive use
   of typeclass patterns (Equiv / Order / Hashing / ...).
  
  - Macros sparingly used to boost performance on Int-indexed structures.

Implicit conversions from Scala & Java collections to Poly collections are provided by importing the
objects `poly.collection.conversion.{Java, Scala}`.

## Installation

```scala
libraryDependencies += "me.tongfei" %% "poly-collection" % "0.1.0"
```

