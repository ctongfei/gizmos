package poly.collection

import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object BitSetTest extends App {


  val bs = BitSet()


  bs.add_!(3)
  bs.add_!(4)
  bs.add_!(5)

  bs.remove_!(3)
  bs.add_!(3)


  for (i ← bs.keys) println(i)


}
