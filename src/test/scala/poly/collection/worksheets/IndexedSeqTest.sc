import poly.collection._
import poly.collection.mut._
import poly.algebra._
import poly.algebra.syntax._

val a = ArraySeq(1, 3, 4, 2, 0).asIndexedSeq
val b = ArraySeq("a", "b").asIndexedSeq
val e = ArraySeq().asIndexedSeq

a.length
b.length
e.length
a.sizeKnown
b.sizeKnown
e.sizeKnown

a(3)
b(0)

a foreach print
b foreach print
e foreach print

a.headNode
b.headNode
a.lastNode
a.dummy
b.dummy
e.dummy

a.pairs
b.pairs
e.pairs

a.keys
b.keys
e.keys

a.map(_ * 2)
b.map(_.length)
e.map(_ => 1)

a monadicProduct b
a monadicProduct e

a product b

// PCOLL-1 (a product e)

a :+ 4
4 +: a
b :+ "!"
"?" +: b
e :+ 1
1 +: e

a ++ b
a ++ e
e ++ a

a reduce {_+_}
b reduce {_++_}

a.head
b.head
a.last
b.last
a.init
b.init
a.tail
b.tail

a.prefixes
a.suffixes

a take 3
a skip 2
a slice (1, 3)

a slice (0, -1)
a slice (0, -2)

a rotate 2
a rotate -1

a.reverse
b.reverse

a repeat 3
b repeat 0
e repeat 4

a.sliding(2)
b.sliding(3)
e.sliding(1)
a.sliding(2, 2)

a interleave b

a zip b
(a zipWith b)(_.toString + _)
b zip e

a permuteBy (Permutation(1, 2, 3, 4, 0))
