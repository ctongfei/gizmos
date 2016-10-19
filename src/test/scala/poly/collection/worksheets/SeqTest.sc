import poly.algebra.syntax._
import poly.collection._
import poly.collection.mut._

val a = ListSeq(0, 1, 2, 3, 4, 3, 2).asSeq
val b = ArraySeq(5, 6, 7, 8).asSeq
val c = immut.List(1, 2, 3, 4)

b.asMap

a collect { case x if x < 3 => x * x }

a startsWith ListSeq(0, 1, 2, 3, 4, 3, 2, 0)

a endsWith ListSeq()

a.pairs to Map

a.take(3)
a.takeWhile(_ < 3)
a.takeTo(_ == 2)
a.takeUntil(_ > 3)
a.take(10)
a.drop(2)
a.drop(10)
a.dropWhile(_ > 100)
a.flatMap((i: Int) => Seq(i) repeat i)
a product b
a.isEmpty
a.size

a.count(_ == 2)
a.filter(_ >= 2.4)
a concat b
a prepend -1
a append -1
a reduce (_+_)
a.scan(0)(_+_)
a.scanRight(0)(_+_)
a.consecutive(_-_)
a.head
a.tail
a.init
a.suffixes
a.prefixes
a.slice(1, 2)
a.rotate(2)
a zip b
a.sliding(2)
a.firstIndexOf(2)
a.firstIndexWhere(_ == 2)

