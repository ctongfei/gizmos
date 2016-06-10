##### \[IMPROVEMENT\] Negative indices for `Seq`s
    CHECK bugs

##### \[IMPROVEMENT\] Support for negative indices for `rotate` 
    CHECK bugs

##### \[IMPROVEMENT\] Path-dependent types for SeqNodes/BinaryTreeNodes/TreeNodes

##### \[CHECK?\] Depedent-typed object's equals/hashCode

##### \[IMPROVEMENT\] Dense Int-keyed Set/Map/Table/Graph builder

##### \[FUNCTION\] Deques / PriorityDeques
ArrayDeque / MinMaxHeapPriorityDeque ?

##### \[FUNCTION\] More heaps & mergeable heaps
LeftistHeap / PairingHeap / BinomialHeap / FibonacciHeap ?

##### \[BUG\] Map's hashCode and Seq's are not compatible
```scala
val x: Seq[Int]
Map.Hashing.hash(x) should_== Seq.Hashing.hash(x)
```
But currently this is not the case.

##### \[IMPROVEMENT\] Implicit resolution of contravariant typeclass instances (SI-2509)

Map.Eq / Seq.Eq / IndexedSeq.Eq 

```scala
def f: Iterable[IndexedSeq[Int]]

f.group // should find IndexedSeq.Eq, but static resolution failed
```

Current workaround: using dynamic resolution
```scala
implicit def __eq[K, V: Eq]: Eq[Map[K, V]] = new Eq[Map[K, V]] {
    def eq(x: Map[K, V], y: Map[K, V]) = (x, y) match {
        case (x: IndexedSeq[V], y: IndexedSeq[V]) => IndexedSeq.Eq[V].eq(x, y)
        ...
    }
}
```

This is not optimal.
