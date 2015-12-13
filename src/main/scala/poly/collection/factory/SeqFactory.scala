package poly.collection.factory

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
 * Contains common constructors for sequences.
 * This trait should be inherited by companion objects of sequence implementation classes.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SeqFactory[+C[_]] extends CollectionFactory[C] {

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
