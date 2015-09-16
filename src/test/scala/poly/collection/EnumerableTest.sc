import poly.collection._
import poly.algebra._
import poly.algebra.implicits._
import poly.collection.mut._
import poly.collection.ops._

val e = Iterable.iterate(0)(_ + 1).take(5)
val f = Iterable.iterate(0)(x => 0).take(10)
e.head
e.tail
f
e.sort
e :+ 0
0 +: e

e.reduce(_+_)


e.to[ArraySeq]
e.to[ListSeq]
e.to[ListBiSeq]
e.reverse
e
e.map(_ + 1)
e.flatMap((i: Int) => ListSeq.fill(i)(i))
e.prepend(-1).map(_ * 2).filter(_ >= 0)
e.append(19)
val gg = ArraySeq(1, 2, 3)
val g = e.to[ListSeq]
g.filter(_ > 2)
g.filter(_ < 4)
g.map(_ * 3)
gg.map(_ * 3)
gg.sort(WeakOrder[Int].reverse)
g cartesianProduct gg

e.sliding(2)

e.tail
e.init

0.infinite.take(10)

