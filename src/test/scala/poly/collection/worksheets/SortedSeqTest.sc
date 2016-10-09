import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.mut._

val a = SortedArraySeq(1, 2, 3, 4, 4, 4, 5)(Order[Int].reverse)

a.lowerBound(4)
a.upperBound(4)

a.distinct
