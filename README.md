## Poly-collection

Poly-collection is a Scala collection framework that aims to provide an alternative to the standard
 Scala collection framework. It provides a large range of collection classes, and differs from
 the standard collection framework in several ways:
 
  - A large collection: full support for trees and graphs.

  - Always non-strict when using higher-order functions: functions like `map`, `filter`, `flatMap`
  are always evaluated lazily. This allows chain application like `xs.map(f).filter(g).reduce(h)`
  to be much faster than the standard library.

  - Full support for algebraic structures through [poly-algebra](https://github.com/ctongfei/poly-algebra).

Implicit conversions from Scala & Java collections to Poly collections are provided by importing the
package `poly.collection.conversion._`.

Package `poly.collection.mutable` is what you would like to import. It contains the following mutable
collection classes:

 - `ArraySeq`, `ListSeq`, `SortedArraySeq`
 - `ArrayStack`, `ArrayQueue`, `BinaryHeapPriorityQueue`
 - `HashSet`, `RbTreeSet`, `ListSet`, `SortedArraySet`, `BitSet`
 - `HashMap`, `RbTreeMap`, `ListMap`, `SortedArrayMap`
 - `HashBiMap`


A future package `poly.collection.persistent` is in progress (features persistent, pure immutable data structures).

## Installation

SBT:

```scala
libraryDependencies += "me.tongfei" %% "poly-collection" % "0.1.0"
```
