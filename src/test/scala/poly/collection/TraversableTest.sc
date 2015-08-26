import poly.collection.mut._
import poly.collection._
import poly.algebra._
import poly.algebra.implicits._

val l = ArraySeq(5, 4, 3, 2, 1, 0)
val f = (x: Int) => ArraySeq(x, x)
l.map(_ * 2)
l.flatMap((x: Int) => ArraySeq.fill(x)(x))
l.count(_ == 3)
l.filter(_ > 2)
l.filterNot(_ > 2)
l.scan(0)(_+_)
l.diff(_-_)
l.scan(0)(_+_).diff(_-_)
l.sliding(3)
l.prefixSums
l.prefixSums.differences
