package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.factory._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uV}

/**
 * Basic trait for Poly-collection traversable collections.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Traversable[+T] { self =>

  /**
   * Applies a function ''f'' to each element of this traversable collection.
   * @param f The function to be applied. Return values are discarded.
   * @tparam V Type of the result of function ''f''
   */
  def foreach[V](f: T => V): Unit

  /**
   * Returns a new traversable collection by applying a function to all elements in this collection.
   * @param f Function to apply
   * @tparam U Type of the image of the function
   * @return A new traversable that each element is the image of the original element applied by ''f''.
   */
  def map[U](f: T => U): Traversable[U] = new Traversable[U] {
    def foreach[V](g: U => V) = {
      for (x ← self) g(f(x))
    }
  }

  def flatMap[U](f: T => Traversable[U]) = new Traversable[U] {
    def foreach[V](g: U => V): Unit = {
      for (x ← self)
        for (y ← f(x))
          g(y)
    }
  }

  def filter(f: T => Boolean) = new Traversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (f(x)) g(x)
    }
  }

  def concat[U >: T](that: Traversable[U]) = new Traversable[U] {
    def foreach[V](f: U => V): Unit = {
      for (x ← self)
        f(x)
      for (x ← that)
        f(x)
    }
  }

  def exists(f: T => Boolean): Boolean = {
    for (x ← self)
      if (f(x)) return true
    false
  }

  def forall(f: T => Boolean): Boolean = {
    for (x ← self)
      if (!f(x)) return false
    true
  }

  def count(f: T => Boolean): Int = {
    var s = 0
    for (x ← self)
      if (f(x)) s += 1
    s
  }

  def size: Int = {
    var s = 0
    for (x ← self) s += 1
    s
  }

  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  def foldLeft[U](z: U)(f: (U, T) => U): U = {
    var r = z
    for (x ← self)
      r = f(r, x)
    r
  }

  def foldRight[U](z: U)(f: (T, U) => U): U = {
    ???
  }

  def reduce[U >: T](f: (U, U) => U): U = reduceLeft(f)

  def reduceLeft[U >: T](f: (U, T) => U): U = {
    var first = true
    var res = default[U]
    for (x ← self) {
      if (first) {
        res = x
        first = false
      }
      else res = f(res, x)
    }
    res
  }

  def head: T = {
    for (x ← self)
      return x
    throw new NoSuchElementException
  }

  def tail = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      var first = true
      for (x ← self) {
        if (!first) f(x)
        first = false
      }
    }
  }

  def last: T = {
    var p = head
    for (x ← this) p = x
    p
  }

  def init = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      var p = head
      var followed = false
      for (x ← self) {
        if (followed) f(p)
        else followed = false
        p = x
      }
    }
  }

  def take(n: Int) = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        f(x)
        i += 1
        if (i >= n) return
      }
    }
  }

  def drop(n: Int) = new Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        if (i >= n) f(x)
        i += 1
      }
    }
  }

  def takeWhile(f: T => Boolean) = new Traversable[T] {
    def foreach[U](g: T => U): Unit = {
      for (x ← self) {
        if (!f(x)) return
        g(x)
      }
    }
  }

  def dropWhile(f: T => Boolean) = new Traversable[T] {
    def foreach[U](g: T => U): Unit = {
      var starts = false
      for (x ← self) {
        if (!starts && !f(x)) starts = true
        if (starts) g(x)
      }
    }
  }

  def slice(i: Int, j: Int) = drop(i).take(j - i)

  def find(f: T => Boolean): Option[T] = {
    for (x ← self)
      if (f(x)) return Some(x)
    None
  }

  def sum[X >: T](implicit G: AdditiveSemigroup[X]): X = reduce(G.add)

  def product[X >: T](implicit G: MultiplicativeGroup[X]): X = reduce(G.mul)

  def min[X >: T](implicit O: WeakOrder[X]): X = ??? //TODO: reduce(O.min)

  def max[X >: T](implicit O: WeakOrder[X]): X = ??? //TODO: reduce(O.max)

  def argmin[U: WeakOrder](f: T => U): T = argminWithValue(f)._1

  def argmax[U: WeakOrder](f: T => U): T = argmaxWithValue(f)._1

  def argminWithValue[U](f: T => U)(implicit O: WeakOrder[U]): (T, U) = {
    var minKey = default[T]
    var minVal = default[U]
    var first = true

    for (x ← self) {
      val fx = f(x)
      if (first | fx < minVal) {
        minKey = x
        minVal = fx
        first = false
      }
    }
    (minKey, minVal)
  }

  def argmaxWithValue[U](f: T => U)(implicit O: WeakOrder[U]): (T, U) = {
    var maxKey = default[T]
    var maxVal = default[U]
    var first = true

    for (x ← self) {
      val fx = f(x)
      if (first | fx > maxVal) {
        maxKey = x
        maxVal = fx
        first = false
      }
    }
    (maxKey, maxVal)
  }

  def mkString(delimiter: String): String = {
    val sb = new StringBuilder
    sb.append(this.head)
    for (x ← tail) {
      sb.append(delimiter)
      sb.append(x)
    }
    sb.result()
  }

  def to[C[_]](implicit builder: Builder[T @uV, C[T @uV]]): C[T @uV] = {
    val b = builder
    b ++= self
    b.result
  }

}
