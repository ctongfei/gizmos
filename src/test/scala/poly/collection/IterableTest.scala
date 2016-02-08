package poly.collection

import poly.collection.ops._
import org.scalatest._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class IterableTest extends FunSuite {

  import TestUtil._

  test("Int sequence") {

    val n = 1000
    val s = scala.Iterable.iterate(1, n)(i => (i + 10000) % 997)
    val p = Iterable.iterate(1)(i => (i + 10000) % 997).take(n)

    val z = Iterable.zipN(p, p, p)
    z foreach println

    val h = (1, 1).iterate { case (a, b) => (a + 1, b - 1) }.take(10)


    s ==?== p
    s.map(_ * 2) ==?== p.map(_ * 2)
    s.drop(4) ==?== p.skip(4)
    s.take(8) ==?== p.take(8)

  }

}
