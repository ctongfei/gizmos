import poly.collection._
import poly.collection.mut._

val b = BitSet()

b.addInplace(4)
b.addInplace(5)
b.removeInplace(4)
b

b += 3
b

b ++= Seq(1, 2, 3, 4, 5)
b

