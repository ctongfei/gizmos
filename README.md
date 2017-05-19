## Poly-collection

Poly-collection is a Scala collection framework that aims to provide an alternative to the standard
 Scala collection framework. It provides a large range of collection classes, and differs from
 the standard collection framework in several ways:
 
  - Full support for **graphs, trees, multisets, multimaps** and others with various functional operators defined on them.
  
  - **Based on algebraic typeclasses through the Typelevel Scala family projects
   [Cats](https://github.com/typelevel/cats), [Algebra](https://github.com/typelevel/algebra) and [Spire](https://github.com/non/spire)**
    (`Eq`/`Order`/...). This means the equivalence relation / hashing function for sets / maps 
     can be customized via arbitrary typeclass instances.
     
  - **Lazy by default**: Higher-order transformational functions are nearly always executed in a non-strict manner:
   functions like `map`, `filter`, `flatMap` are nearly always evaluated lazily (like Spark). This allows chain application like 
   `xs map f filter g reduce h` to be much faster than the standard library.
  
  - **Returning-same-trait principle**: Instead of the uniform-return-type principle of the Scala standard collection
   library, Poly-collection returns the most fined-grained trait possible without much performance loss.
    For example, a `map` applied on an `ArraySeq` would return a read-only view, 
    or a lazy `IndexedSeq` instead of a strict `ArraySeq`.
  
  - **`Map`s and `Set`s are not `Iterable`s**, instead, `map.pairs` or `set.keys` are. The nonsensical behavior of `zip`/`tail` etc. 
  for Sets/Maps in Scala is removed. `map.zip` now means zip-by-keys. 

  - Various algorithms / data structures provided out of the box: binary search, disjoint sets, Fenwick trees, etc.
  
    - A fully-fledged `search` subpackage that enables the execution of common search algorithms (BFS, DFS, A*, etc.) on very
   generic state spaces (of course lazily!).
   
    - Caching utilities: `LRUCache`, `Memo`.
  
Implicit conversions from/to Scala & from Java collections to Poly-collections are provided by importing the
objects `poly.collection.conversion.{FromJava, FromScala, ToScala}`.

Poly-collection is currently in alpha stage and is progressing into the 0.1 release.

```scala
libraryDependencies += "me.tongfei" %% "poly-collection" % "0.1.0"
```

#### Pending Scala issues
 - [SI-2509: Contravariant implicits](https://issues.scala-lang.org/browse/SI-2509) influences the use of contravariant typeclasses `Eq`/`Order`/`Hashing`.
 - [SI-2712: Higher-order unification](https://issues.scala-lang.org/browse/SI-2712) influences the `to` method on `Traversable`s when piping to a `BitSet`/`DenseIntKeyedMap`.
  
#### Todo
 - Remove Spire from the dependencies.
 