package poly.collection

import org.scalatest._
import poly.algebra.syntax._
import poly.collection.testutil._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class IterableTest extends FunSuite {

  import TestUtil._

  implicit class Ops[T](val s: scala.collection.Iterable[T]) {
    def ~~(p: poly.collection.Iterable[T]) = checkIterable(s, p)
  }
  
  val n = 1000
  val s = scala.collection.Iterable.iterate(1, n)(i => (i + 10000) % 997)
  val p = poly.collection.Iterable.iterate(1)(i => (i + 10000) % 997).take(n)

  test("Iterable equality") { s ~~ p }

  test("Iterable.map") { s.map(_ * 2) ~~ p.map(_ * 2) }

  test("Iterable.product") { (for (x ← s; y ← s) yield (x, y)) ~~ (p product p) }

  test("Iterable.filter") { s.filter(_ < 400) ~~ p.filter(_ < 400) }
  test("Iterable.filterNot") { s.filterNot(_ % 3 == 0) ~~ p.filterNot(_ % 3 == 0) }

  test("Iterable.concat") { (s ++ s) ~~ (p concat p) }
  test("Iterable.append") { (s ++ scala.Iterable(3)) ~~ (p :+ 3) }
  test("Iterable.prepend") { (scala.Iterable(3) ++ s) ~~ (3 +: p) }

  test("Iterable.scanLeft") { s.scanLeft(0)(_+_) ~~ p.scanLeft(0)(_+_) }
  test("Iterable.scanRight") { s.scanRight(0)(_+_) ~~ p.scanRight(0)(_+_) }


  test("Iterable.skip") { s.drop(4) ~~ p.drop(4) }
  test("Iterable.take") { s.take(8) ~~ p.take(8) }

}
