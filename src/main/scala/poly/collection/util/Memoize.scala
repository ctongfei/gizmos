package poly.collection.util

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.mut._

/**
 * Utility for memoizing a recursive function.
 * @author Tongfei Chen
 */
class Memoize[A: Eq, B](f: A => B) extends (A => B) {
  private val cache = AutoMap[A, B]()
  def apply(x: A) = cache getOrElseUpdate (x, f(x))
}

object Memoize {

  def memoize[A: Eq, B](f: A => B) = new Memoize(f)

}
