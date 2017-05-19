package poly.collection.cache

import poly.collection._
import poly.collection.mut._
import poly.collection.typeclass._

/**
 * Encapsulates a memoized function.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class Memo[A, +R] private(f: A => R, c: KeyMutableMap[A, R]) extends CachedFunction[A, R] {

  def apply(a: A) = c getOrElseUpdate (a, f(a))

  def cache: Map[A, R] = c

  /** Clears the cache of this memoized function. */
  def clearCache_!() = c.clear_!()

}

object Memo {

  /**
   * Returns a memoized version of a unary function.
   * @note The equivalence relation on inputs should preferably be an instance of [[Hashing]] to ensure fast memoization.
   * @example A memoized recursive implementation of a Fibonacci sequence: {{{
   * val f: Int => Int = Memo {
   *   case 0 => 0
   *   case 1 => 1
   *   case i => f(i - 1) + f(i - 2)
   * }
   * }}}
   */
  def apply[K: Eq, R](f: K => R) = new Memo(f, AutoMap[K, R]())

  /**
   * Creates an memoized version of a function using the default `hashCode` method on inputs
   * as the hashing function for the memo.
   */
  def byDefaultHashing[K, R](f: K => R) = {
    implicit val eq = Hashing.default[K]
    new Memo(f, HashMap[K, R]())
  }

  /**
   * Creates an memoized version of a function with the by-reference hash function being the
   * hash function for the memo.
   */
  def byRefHashing[K <: AnyRef, R](f: K => R) = {
    implicit val eq = Hashing.byRef[K] // use this as the Eq instance
    new Memo(f, HashMap[K, R]())
  }

  /**
   * Creates an memoized version of a function with a custom cache (should be of type [[KeyMutableMap]]).
   */
  def withCustomCache[K, R](c: KeyMutableMap[K, R])(f: K => R) = new Memo(f, c)

}
