package poly.collection.util

import poly.algebra._
import poly.collection.mut._

object memoized {

  private[poly] class MemoizedFunction1[A: Eq, R](f: A => R) extends (A => R) {
    private val cache = AutoMap[A, R]()
    def apply(a: A) = cache getOrElseUpdate (a, f(a))
  }

  private[poly] class MemoizedFunction2[A: Eq, B: Eq, R](f: (A, B) => R) extends ((A, B) => R) {
    private val cache = AutoMap[(A, B), R]()(Eq.onTuple2[A, B])
    def apply(a: A, b: B) = cache getOrElseUpdate ((a, b), f(a, b))
  }

  private[poly] class MemoizedFunction3[A: Eq, B: Eq, C: Eq, R](f: (A, B, C) => R) extends ((A, B, C) => R) {
    private val cache = AutoMap[(A, B, C), R]()(Eq.onTuple3[A, B, C])
    def apply(a: A, b: B, c: C) = cache getOrElseUpdate ((a, b, c), f(a, b, c))
  }

  /**
   * Utility for memoizing a recursive function.
   *
   * @example A memoized recursive implementation of a Fibonacci sequence: {{{
   * val f: Int => Int = memoized {
   *   case 0 => 0
   *   case 1 => 1
   *   case i => f(i - 1) + f(i - 2)
   * }
   * }}}
   * @since 0.1.0
   * @author Tongfei Chen
   */
  def apply[A: Eq, R](f: A => R) = new MemoizedFunction1(f)

  def apply[A: Eq, B: Eq, R](f: (A, B) => R) = new MemoizedFunction2(f)

  def apply[A: Eq, B: Eq, C: Eq, R](f: (A, B, C) => R) = new MemoizedFunction3(f)

}
