package poly.collection

import poly.algebra.syntax._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object BitSetTest extends App {

  val a = BitSet(3, 4, 5)
  val b = BitSet(5, 6, 7)

  val c = BitSet from Seq(1, 2, 3, 4, 5, 13, 25)
  val d = Seq(1, 2, 3, 8, 10).to[BitSet]

  val bp = 0


}
