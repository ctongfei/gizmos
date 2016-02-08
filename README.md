## Poly-collection

Poly-collection is a Scala collection framework that aims to provide an alternative to the standard
 Scala collection framework. It provides a large range of collection classes, and differs from
 the standard collection framework in several ways:
 
  - Full support for trees and graphs, with all kinds of functional operators defined on them.
  
  - Lazy by default: Higher-order transformational functions are nearly always executed in a non-strict manner:
   functions like `map`, `filter`, `flatMap` are nearly always evaluated lazily. This allows chain application like 
   `xs.map(f).filter(g).reduce(h)` to be much faster than the standard library.
  
  - Returning-same-trait principle: Instead of the uniform-return-type principle of the Scala standard collection
   library, Poly-collection returns the most fined-grained trait possible without much performance loss. This eliminates
   a lot of unnecessary intermediate structure building. For example, a `map` applied on an `ArraySeq` would return a read-only
   view, or a lazy `IndexedSeq` instead of a full-fledged `ArraySeq`.
  
  - Various algorithms provided out of the box: binary search; disjoint sets; A* search; ...

  - Concretely based on basic algebraic concepts through [poly-algebra](https://github.com/ctongfei/poly-algebra) by extensive use
   of typeclass patterns (Equiv / WeakOrder / IntHashing / ...).
  
  - Macros sparingly used to boost performance on Int-indexed structures.
  
  - For other design principles, please see the wiki of this library.

Implicit conversions from/to Scala & from Java collections to Poly collections are provided by importing the
objects `poly.collection.conversion.{FromJava, FromScala, ToScala}`.

Poly-collection is currently in alpha stage and is progressing into the 0.1 release.
The current version is 0.1.0 Milestone 1.

## Installation

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "me.tongfei" %% "poly-collection" % "0.1.0-M1-SNAPSHOT"
```

## Future work

 - More specialization to boost the performance
 
 - Parallel collections
 
 - ...
 