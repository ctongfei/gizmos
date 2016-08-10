package poly.collection.cache

import poly.algebra._
import poly.collection._
import poly.collection.mut._

/**
 * Utility for memoizing a recursive function.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class Memoized[A: Eq, +R] private(f: A => R) extends CachedFunction[A, R] {

  private[this] val c = AutoMap[A, R]()

  def apply(a: A) = c getOrElseUpdate (a, f(a))

  def cache: Map[A, R] = c
}

object Memoized {

  /** Returns a memoized version of a unary function.
   * @example A memoized recursive implementation of a Fibonacci sequence: {{{
   * val f: Int => Int = Memoized {
   *   case 0 => 0
   *   case 1 => 1
   *   case i => f(i - 1) + f(i - 2)
   * }
   * }}}
   */
  def apply[A: Eq, R](f: A => R) = new Memoized(f)

  /**
   * Creates an memoized version of a function using the default `hashCode` method on inputs
   * as the hashing function for the memo.
   */
  def byDefaultHashing[A, R](f: A => R) = new Memoized(f)(Hashing.default[A])

}
