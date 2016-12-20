## Poly-collection

Poly-collection is a Scala (2.11/2.12) collection framework that aims to provide an alternative to the standard
 Scala collection framework. It provides a large range of collection classes, and differs from
 the standard collection framework in several ways:
 
  - Full support for graphs, trees, multisets, multimaps and others with various functional operators defined on them.
  
  - Concretely based on basic algebraic concepts through [Poly-algebra](https://github.com/ctongfei/poly-algebra) by extensive use
     of typeclass patterns (`Eq`/`Order`/`Hashing`/...). This means the equivalence relation / hashing function for sets / maps 
     can be customized via arbitrary typeclass instances.
     
  - Lazy by default: Higher-order transformational functions are nearly always executed in a non-strict manner:
   functions like `map`, `filter`, `flatMap` are nearly always evaluated lazily (like Spark). This allows chain application like 
   `xs map f filter g reduce h` to be much faster than the standard library.
  
  - Returning-same-trait principle: Instead of the uniform-return-type principle of the Scala standard collection
   library, Poly-collection returns the most fined-grained trait possible without much performance loss.
    For example, a `map` applied on an `ArraySeq` would return a read-only view, 
    or a lazy `IndexedSeq` instead of a strict `ArraySeq`.
  
  - `Map`s and `Set`s are not `Iterable`s. The nonsensical behavior of `zip`/`tail` etc. for Sets/Maps in Scala is removed.
    Use `map.pairs` or `set.keys` to get the original Scala behavior.

  - Various algorithms / data structures provided out of the box: binary search, disjoint sets, Fenwick trees, etc.
  
    - A fully-fledged `search` subpackage that enables the execution of common search algorithms (BFS, DFS, A*, etc.) on very
   generic state spaces (of course lazily!).
   
    - Caching utilities: `LRUCache`, `Memoized`.
  
Implicit conversions from/to Scala & from Java collections to Poly-collections are provided by importing the
objects `poly.collection.conversion.{FromJava, FromScala, ToScala}`.

Poly-collection is currently in alpha stage and is progressing into the 0.1 release.
The current version is 0.1-SNAPSHOT.

### Installation

```scala
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "me.tongfei" %% "poly-collection" % "0.1.0-SNAPSHOT"
```

### Documentation
TODO

#### Pending Scala issues
 - [SI-2509: Contravariant implicits](https://issues.scala-lang.org/browse/SI-2509) influences the use of contravariant typeclasses `Eq`/`Order`/`Hashing`.
 - [SI-2712: Higher-order unification](https://issues.scala-lang.org/browse/SI-2712) influences the `to` method on `Traversable`s when piping to a `BitSet`/`DenseIntKeyedMap`.
  
 
 
 