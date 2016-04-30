package poly.collection

/**
 * @author Tongfei Chen
 */
object ShuffleTest extends App {

  import poly.algebra.syntax._
  import poly.collection.mut._

  val a = ArraySeq(1, 2, 3)

  val b = Iterable.infinite(a.shuffle).take(10000).group(IndexedSeq.Equiv[Int]).map(_.size)

  val bp = 0

}
