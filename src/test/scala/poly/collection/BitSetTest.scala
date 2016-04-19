package poly.collection

import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object BitSetTest extends App {

  val bs = BitSet()

  bs.add(3)
  bs.add(4)
  bs.add(5)

  bs.remove(3)
  bs.add(3)


  for (i ← bs.keys) println(i)


}
