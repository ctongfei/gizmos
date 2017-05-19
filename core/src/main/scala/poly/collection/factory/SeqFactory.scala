package poly.collection.factory

import poly.collection._

import scala.language.higherKinds

/**
  * Contains common constructors for sequences.
  * This trait should be inherited by companion objects of sequence implementation classes.
  * @author Tongfei Chen
 */
trait SeqFactory[+C[_]] extends Factory1[Id, C, Trivial.P1] {

  final def newBuilder[T : Trivial.P1] = newSeqBuilder[T]

  def newSeqBuilder[T]: Builder[T, C[T]]

  /**
   * Generates an empty sequence, but size hinted (ensures enough space for sequences such as [[mut.ArraySeq]]).
   */
  def withSizeHint[T](n: Int): C[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    b.result
  }

  /**
   * Generates a sequence containing a specific element (call-by-name: each time computed differently) multiple times.
   * @example `Seq.fill(3)(random)` may be `(0.767, -0.331, 0.016)`
   */
  def fill[T](n: Int)(x: => T): C[T] = {
    var i = n
    val b = newBuilder[T]
    b.sizeHint(n)
    while (i > 0) {
      b add x
      i -= 1
    }
    b.result
  }

  def tabulate[T](n: Int)(f: Int => T): C[T] = {
    var i = 0
    val b = newBuilder[T]
    b.sizeHint(n)
    while (i < n) {
      b add f(i)
      i += 1
    }
    b.result
  }

  def iterateN[T](start: T, n: Int)(f: T => T): C[T] =
    unfoldN(start, n)(s => (f(s), s))

  def iterateTo[T](start: T, goal: T => Boolean)(f: T => T): C[T] = {
    val b = newBuilder[T]
    var x = start
    while (!goal(x)) {
      b add x
      x = f(x)
    }
    b add x
    b.result
  }

  def iterateUntil[T](start: T, goal: T => Boolean)(f: T => T): C[T] = {
    val b = newBuilder[T]
    var x = start
    while (!goal(x)) {
      b add x
      x = f(x)
    }
    b.result
  }

  def unfoldN[S, T](start: S, n: Int)(f: S => (S, T)): C[T] = {
    var i = n
    var s = start
    val b = newBuilder[T]
    b.sizeHint(n)
    while (i > 0) {
      val (s1, t) = f(s)
      s = s1
      b << t
      i -= 1
    }
    b.result()
  }

  //todo: unfoldTo, unfoldUntil

}
