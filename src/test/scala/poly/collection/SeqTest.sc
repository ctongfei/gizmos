import poly.collection._
import poly.collection.mut._

val a = ListSeq(0, 1, 2, 0, 3, 4)
val b = ListSeq(5, 6, 7, 8)

a.map(_ + 1)

a concat b

a.flatMap((x: Int) => ListSeq.fill(x)(x))
b.flatMap((x: Int) => a)

a prepend 0

a append 0

val c = ArraySeq(1, 2, 3)
val d :|< (e |: f) = c
