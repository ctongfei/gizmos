import poly.collection.mut._
import poly.collection._
import poly.algebra._
import poly.algebra.implicits._

val a = ArraySeq(5, 3, 3, 2, 1, 0).asTraversable
val b = ArraySeq('a', 'b', 'c').asTraversable
val c: Traversable[Int] = ArraySeq().asTraversable
val f = (x: Int) => ArraySeq(x, x).asTraversable

a.cycle.take(20)

a map {_ * 2}
a flatMap {(x: Int) => ArraySeq.fill(x)(x)}
a monadicProduct b
a monadicProduct c

a.isEmpty
b.isEmpty
c.isEmpty

a.count(_ == 3)
b.count(_ < 'c')


a.filter(_ > 2)
a.filterNot(_ > 2)
c.filter(_ != 0)

a.filterMany(_ > 1, _ > 2, _ > 3)
a.partition(_ > 3)
a.findFirst(_ < 3)
a.findFirst(_ < 0)
c.findFirst(_ < 0)

a ++ b
a ++ c
a ++ a ++ a

a :+ 1
1 +: a
0 +: c
c :+ 0

a.foldLeft(-1)(_+_)
a.foldRight(-1)(_+_)


a.scan(0)(_+_)
a.consecutive(_-_)
a.scan(0)(_+_).consecutive(_-_)
//l.sliding(3)
a.prefixSums
a.prefixSums.differences

a.head
a.tail
a.init
a.last
a.suffixes
a.prefixes
a skip 2
a take 3
a take 7
a skip -1
a skip 8
a.skipWhile(_ >= 3)
a.takeWhile(_ >= 3)
a.takeTo(_ == 2)
a.takeUntil(_ <= 3)
a.slice(3, 4)
a.rotate(3)
a.reverse
a.sort
a.sort(Order[Int].reverse)
a.forall(_ >= 0)
a.forall(_ >= 3)
a.exists(_ >= 3)
a.exists(_ >= 7)
a.sum
a.max
a.min
a.argmax(-_)
a.argmin(-_)
a.argmaxWithValue(_ * 2)
val d = ArraySeq(ArraySeq(1, 2, 3).asTraversable,
  ArraySeq(3, 4, 5).asTraversable).asTraversable
val e = ArraySeq((1, 'a'), (2, 'b')).asTraversable
d.flatten
e.unzip
e.unzipEagerly

a.cycle.map(_ - 1).take(10)

