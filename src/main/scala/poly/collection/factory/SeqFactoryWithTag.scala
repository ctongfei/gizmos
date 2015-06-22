package poly.collection.factory

import poly.collection._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.reflect._



trait SeqFactoryWithTag[C[_]] extends CollectionFactoryWithTag[C] {

  def withSizeHint[T: ClassTag](n: Int): C[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    b.result
  }

  def fill[@sp(fdi) T: ClassTag](n: Int)(x: => T): C[T] = {
    var i = n
    val b = newBuilder[T]
    b.sizeHint(n)
    while (i > 0) {
      b += x
      i -= 1
    }
    b.result
  }

  def tabulate[@sp(fdi) T: ClassTag](n: Int)(f: Int => T): C[T] = {
    var i = 0
    val b = newBuilder[T]
    b.sizeHint(n)
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result
  }

  def iterate[@sp(fdi) T: ClassTag](start: T, n: Int)(f: T => T): C[T] = {
    var i = n
    val b = newBuilder[T]
    b.sizeHint(n)
    if (i > 0) {
      var x = start
      b += x
      i -= 1
      while (i > 0) {
        x = f(x)
        b += x
        i -= 1
      }
    }
    b.result
  }

  def iterateUntil[@sp(fdi) T: ClassTag](start: T, goal: T => Boolean)(f: T => T): C[T] = {
    val b = newBuilder[T]
    var x = start
    while (!goal(x)) {
      b += x
      x = f(x)
    }
    b.result
  }

}
