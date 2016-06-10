package poly.collection.util

import poly.algebra._
import poly.collection.mut._

private[poly] class Memoized[A: Eq, B](f: A => B) extends (A => B) {
  private val cache = AutoMap[A, B]()
  def apply(x: A) = cache getOrElseUpdate (x, f(x))
}

/**
 * Utility for memoizing a recursive function.
 *
 * @example A memoized recursive implementation of a Fibonacci sequence: {{{
    val f: Int => Int = memoized {
      case 0 => 0
      case 1 => 1
      case i => f(i - 1) + f(i - 2)
    }
 * }}}
 * @since 0.1.0
 * @author Tongfei Chen
 */
object memoized {

  def apply[A: Eq, B](f: A => B) = new Memoized(f)

}