import poly.collection._
import poly.collection.mut._

val a = ArraySeq(1, 2, 3).asBiSeq

a.reverse
a.map(_ * 2).reverse

a.foldRight(0)(_+_)

a.suffixes
a.prefixes

a.rotate(2)

a.scanLeft(1)(_+_).slidingPairsWith(_-_)



