import poly.collection.mut._

val b = BitSet()

b.addInplace(4)
b.addInplace(5)
b.removeInplace(4)
b foreach println

