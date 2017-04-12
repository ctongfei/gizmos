package poly.collection.cache

import poly.algebra._
import poly.collection._
import poly.collection.mut._

/**
 * Memoized wrapper of a function.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class Memoized[K: Eq, +R] private(f: K => R) extends CachedFunction[K, R] {

  private[this] val c = AutoMap[K, R]()

  def apply(a: K) = c getOrElseUpdate (a, f(a))

  def cache: Map[K, R] = c

  def clearCache_!() = c.clear_!()
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
  def apply[K: Eq, R](f: K => R) = new Memoized(f)

  /**
   * Creates an memoized version of a function using the default `hashCode` method on inputs
   * as the hashing function for the memo.
   */
  def byDefaultHashing[K, R](f: K => R) = new Memoized(f)(Hashing.default[K])

  def byRefHashing[K <: AnyRef, R](f: K => R) = new Memoized(f)(Hashing.byRef[K])

}
