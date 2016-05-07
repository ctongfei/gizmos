package poly.collection

import poly.algebra._

import scala.collection._

/**
 * @author Tongfei Chen
 */
object ShuffleTest extends App {

  import poly.algebra.syntax._
  import poly.collection.mut._

  val a = ArraySeq(1, 2, 3, 4)

  val b = Iterable.infinite(a.shuffle).take(240000).to(PairMultiset.of[Int])

  val bp = 0

}
