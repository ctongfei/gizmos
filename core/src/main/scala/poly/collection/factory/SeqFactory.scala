package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.evidence._

import scala.language.higherKinds

/**
  * Contains common constructors for sequences.
  * This trait should be inherited by companion objects of sequence implementation classes.
  * @author Tongfei Chen
 */
trait SeqFactory[+C[_]] extends Factory1[Id, C, NoneEv] {

  def newBuilder[T : NoneEv] = newSeqBuilder[T]

  def newSeqBuilder[T]: Builder[T, C[T]]

  def withSizeHint[T](n: Int): C[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    b.result
  }

  /** Creates a collection by adding the non-null arguments into it. */
  def applyNonNull[T](xs: T*): C[T] = {
    val b = newBuilder[T]
    for (x <- xs if x != null) b add x
    b.result
  }

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

  def iterateN[T](start: T, n: Int)(f: T => T): C[T] = {
    var i = n
    val b = newBuilder[T]
    b.sizeHint(n)
    if (i > 0) {
      var x = start
      b add x
      i -= 1
      while (i > 0) {
        x = f(x)
        b add x
        i -= 1
      }
    }
    b.result
  }

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

}
