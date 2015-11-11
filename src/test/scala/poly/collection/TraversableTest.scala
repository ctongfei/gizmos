package poly.collection

import org.scalatest._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TraversableTest extends FunSuite {

  test("Traversable functions") {
    val a = ArraySeq(5, 3, 3, 2, 1, 0).asTraversable
    val b = a.tails
    val bp = 0
  }

}
