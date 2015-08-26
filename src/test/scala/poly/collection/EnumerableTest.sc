import poly.collection._
import poly.collection.mut._

val e = Enumerable.iterate(0)(_ + 1).take(5)

val f = Enumerable.iterate(0)(x => 0).take(10)

e.head
f
e.to[ArraySeq]

e

e.map(_ + 1)

e.prepend(-1).map(_ * 2).filter(_ >= 0)

e.append(19)
