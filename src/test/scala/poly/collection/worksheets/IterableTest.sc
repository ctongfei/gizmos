import poly.collection._
import poly.algebra._
import poly.algebra.implicits._
import poly.collection.mut._
import poly.collection.ops._

val e = Iterable.iterate(0)(_ + 1).take(5)
val f = Iterable.iterate(0)(x => 0).take(10)
val em = Iterable.empty

e.splitBy(_ == 2)

e.distinct
f.distinct
e union f
e intersect f

e.map(_ + 1)
e.map(_ + 1).map(_ - 2)
e.isEmpty
e.size
e.count(_ < 2)
e.filter(_ > 2)
e.filterNot(_ < 3)
e ++ e ++ e ++ f
e :+ 0
0 +: e

e.scanLeft(0)(_+_)
e.scanByMonoid(AdditiveMonoid[Int].asMonoidWithAdd)
e.consecutive(_-_)



e.head
e.tail
e.sort
e.skip(1)
e.skipWhile(_ < 2)
e.take(2)
e.takeWhile(_ < 3)
e.takeUntil(_ > 3)
e.takeTo(_ > 1)
e.slice(3, 4)
e.rotate(2)
e.cycle.take(11)


e.reduceLeft(_+_)


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
g product gg

e.sliding(2)

e.tail
e.init

0.cycle.take(10)

0.repeat(10)
e.repeat(4)

e.cycle.take(12)

e zip f
e interleave e.init

Iterable.zipN(e, e, e, e)

