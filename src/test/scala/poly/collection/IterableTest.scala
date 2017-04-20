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

  val p = Seq(1, 2, 3).asIterable.dropTo(_ == 3)
  //scala.Iterable(1).iterator.dropWhile()
  println(p)
}
