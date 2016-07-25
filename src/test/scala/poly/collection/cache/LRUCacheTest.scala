package poly.collection.cache

import poly.algebra.syntax._
import poly.collection._
import org.scalatest._

/**
 * @author Tongfei Chen
 */
class LRUCacheTest extends FunSuite {

  test("LRUCache") {

    def f(x: Int) = (1 to x).sum

    val fc = LRUCache(4)(f)

    assert(fc(1) == 1)
    assert(fc.keys == Seq(1))

    assert(fc(2) == 3)
    assert(fc.keys == Seq(1, 2))

    assert(fc(3) == 6)
    assert(fc.keys == Seq(1, 2, 3))

    assert(fc(4) == 10)
    assert(fc.keys == Seq(1, 2, 3, 4))

    assert(fc(5) == 15)
    assert(fc.keys == Seq(2, 3, 4, 5))

    assert(fc(3) == 6)
    assert(fc.keys == Seq(2, 4, 5, 3))

    assert(fc(2) == 3)
    assert(fc.keys == Seq(4, 5, 3, 2))

    assert(fc(1) == 1)
    assert(fc.keys == Seq(5, 3, 2, 1))

  }

}
