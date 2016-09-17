package poly.collection

import org.scalatest._
import poly.collection.testutil._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class IterableTest extends FunSuite {

  import TestUtil._

  val n = 1000
  val s = scala.collection.Iterable.iterate(1, n)(i => (i + 10000) % 997)
  val p = poly.collection.Iterable.iterate(1)(i => (i + 10000) % 997).take(n)

  test("Iterable equality") { s iterable_=== p }

  test("Iterable.map") { s.map(_ * 2) iterable_=== p.map(_ * 2) }

  test("Iterable.product") { (for (x ← s; y ← s) yield (x, y)) iterable_=== (p monadicProduct p) }

  test("Iterable.filter") { s.filter(_ < 400) iterable_=== p.filter(_ < 400) }
  test("Iterable.filterNot") { s.filterNot(_ % 3 == 0) iterable_=== p.filterNot(_ % 3 == 0) }

  test("Iterable.concat") { (s ++ s) iterable_=== (p concat p) }
  test("Iterable.append") { (s ++ scala.Iterable(3)) iterable_=== (p :+ 3) }
  test("Iterable.prepend") { (scala.Iterable(3) ++ s) iterable_=== (3 +: p) }

  test("Iterable.scanLeft") { s.scanLeft(0)(_+_) iterable_=== p.scanLeft(0)(_+_) }
  test("Iterable.scanRight") { s.scanRight(0)(_+_) iterable_=== p.scanRight(0)(_+_) }


  test("Iterable.skip") { s.drop(4) iterable_=== p.drop(4) }
  test("Iterable.take") { s.take(8) iterable_=== p.take(8) }

}
