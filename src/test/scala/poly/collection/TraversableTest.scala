package poly.collection

import org.scalatest._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TraversableTest extends FunSuite {

  test("Traversable functions") {
    val e = Iterable.iterate(0)(_ + 1).take(5)

    val f = Iterable.iterate(0)(x => 0).take(10)

    e.head
    e.tail

    e.map(_ + 1)

    e.prepend(-1).map(_ * 2).filter(_ >= 0)

    e.append(19)

    val g = e.to[ListSeq]

    g foreach println
    g filter (_ > 2) foreach println

    val bp = 0
  }

}
