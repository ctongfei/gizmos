import poly.collection._
import poly.collection.mut._

val b = BitSet()

b.add_!(4)
b.add_!(5)
b.remove_!(4)
b

b += 3
b

b ++= Seq(1, 2, 3, 4, 5)
b

