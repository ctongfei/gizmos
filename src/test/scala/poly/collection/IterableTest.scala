package poly.collection

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

    assert(checkIterable(s, p))
    assert(checkIterable(s.map(_ * 2), p.map(_ * 2)))
    assert(checkIterable(s.drop(4), p.skip(4)))
    assert(checkIterable(s.take(8), p.take(8)))

  }

}
