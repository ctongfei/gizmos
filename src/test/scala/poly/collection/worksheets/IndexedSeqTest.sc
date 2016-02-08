import poly.collection.mut._
import poly.collection._

val a = ArraySeq(1, 2, 3, 4).asIndexedSeq
val b = ArraySeq(0, 0, 0).asIndexedSeq

a.map(_ * 2)

(a cartesianProduct b)

a concat a

a prepend 0
a append 0

a.head
a.tail
a.suffixes

a.last
a.init
a.prefixes

a.reverse
a.rotate(2)
a.repeat(3)

a zip b
a interleave b
