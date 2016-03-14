import poly.algebra.syntax._
import poly.collection.mut._
import poly.collection.ops._

val a = ListSeq(0, 1, 2, 3).asTraversable
val c = ListSeq(2, 3, 5, 6).asTraversable
val b = ListSeq("A a", "B b", "C c").asTraversable
val e = ArraySeq().asTraversable

a map { _ * 2 }
b map { _.length }
e map { _.## }

a union c
a intersect c

a flatMap { i => i.repeat(i) }
b flatMap { s => s.split(' ') }
a product b
a product e
a.isEmpty
b.isEmpty
e.isEmpty
a.size
b.size
e.size
a.count(_ > 2)
b.count(_ > "B")
a filter { _ % 2 == 1 }
b filter { _.startsWith("A") }
e filter { _ equals 1 }
a partition { _ > 2 }
a concat b
a concat e
b concat b
e concat e
a.foldLeft(0)(_+_)
("" /: a)(_ + _.toString)
(a :\ "")(_.toString + _)
a prepend -1
b prepend "!"
e prepend 0
a append 10
b append "D d"
e append 0

