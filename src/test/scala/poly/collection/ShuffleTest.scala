package poly.collection

import poly.algebra.syntax._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object ShuffleTest extends App {

  val b = Seq(1, 2, 3, 4, 5).shuffle.infinitely.take(120000) to PairWeightedSet.of[Int]

  val bp = 0

}
