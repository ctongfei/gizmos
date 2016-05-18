package poly.collection

import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object BitSetTest extends App {

  val bs = BitSet()


  bs.addInplace(3)
  bs.addInplace(4)
  bs.addInplace(5)

  bs.removeInplace(3)
  bs.addInplace(3)


  for (i ← bs.keys) println(i)


}
