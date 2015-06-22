package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Basic trait for Poly-collection traversable collections.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Traversable[@sp(fdi) +T] { self =>

  /**
   * Applies a function ''f'' to each element of this collection.
   * @param f The function to be applied. Return values are discarded.
   * @tparam V Type of the result of function ''f''
   */
  def foreach[V](f: T => V): Unit

  /**
   * Returns a new traversable collection by applying a function to all elements in this collection.
   * Execution: Lazy
   * @param f Function to apply
   * @tparam U Type of the image of the function
   * @return A new collection that each element is the image of the original element applied by ''f''.
   */
  def map[U](f: T => U): Traversable[U] = new Traversable[U] {
    def foreach[V](g: U => V) = {
      for (x ← self) g(f(x))
    }
  }

  def flatMap[U](f: T => Traversable[U]): Traversable[U] = new Traversable[U] {
    def foreach[V](g: U => V): Unit = {
      for (x ← self)
        for (y ← f(x))
          g(y)
    }
  }

  //region Filtering according to predicates

  def count(f: T => Boolean): Int = {
    var s = 0
    for (x ← self)
      if (f(x)) s += 1
    s
  }

  /**
   *
   * Execution mode: Lazy
   * @param f
   * @return
   */
  def filter(f: T => Boolean): Traversable[T] = new Traversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (f(x)) g(x)
    }
  }

  def filterNot(f: T => Boolean): Traversable[T] = new Traversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (!f(x)) g(x)
    }
  }

  /**
   * Execution mode: Eager
   * @param f
   * @return
   */
  def partition(f: T => Boolean): (Traversable[T], Traversable[T]) = {
    val l, r = ListSeq.newBuilder[T]
    for (x ← self)
      if (f(x)) l += x else r += x
    (l.result, r.result)
  }

  /**
   * Execution mode: Eager
   * @param fs
   * @return
   */
  def filterMany(fs: (T => Boolean)*): Seq[Traversable[T]] = {
    val l = ListSeq.fill(fs.length)(ListSeq[T]())
    for (x ← self)
      for (i ← 0 until fs.length) //TODO: optimize using macro
        if (fs(i)(x)) l(i) append x
    l
  }

  def groupBy[K](f: T => K): Map[K, Traversable[T]] = ???
  //endregion

  def concat[U >: T](that: Traversable[U]): Traversable[U] = new Traversable[U] {
    def foreach[V](f: U => V): Unit = {
      for (x ← self)
        f(x)
      for (x ← that)
        f(x)
    }
  }

  def size: Int = {
    var s = 0
    for (x ← self) s += 1
    s
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

  def findFirst(f: T => Boolean): Option[T] = {
    for (x ← self)
      if (f(x)) return Some(x)
    None
  }

  def sum[X >: T](implicit G: AdditiveMonoid[X]): X = reduce(G.add)

  //def sum[X >: T](implicit G: InplaceAdditiveMonoid[X]) = ???

  def product[X >: T](implicit G: MultiplicativeSemigroup[X]): X = reduce(G.mul)

  def min[X >: T](implicit O: WeakOrder[X]): X = reduce(O.min)

  def max[X >: T](implicit O: WeakOrder[X]): X = reduce(O.max)

  def argmin[U: WeakOrder](f: T => U): T = argminWithValue(f)._1

  def argmax[U: WeakOrder](f: T => U): T = argmaxWithValue(f)._1

  def argminWithValue[U](f: T => U)(implicit O: WeakOrder[U]): (T, U) = {
    var minKey = default[T]
    var minVal = default[U]
    var first = true

    for (x ← self) {
      val fx = f(x)
      if (first || fx < minVal) {
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
      if (first || fx > maxVal) {
        maxKey = x
        maxVal = fx
        first = false
      }
    }
    (maxKey, maxVal)
  }

  def buildString(delimiter: String): String = {
    val sb = new StringBuilder
    var first = true
    for (x ← this) {
      if (first) {
        sb.append(x)
        first = false
      } else {
        sb.append(delimiter)
        sb.append(x)
      }
    }
    sb.result()
  }

  /**
   * Converts this traversable sequence to any collection type.
   * @param builder An implicit builder
   * @tparam C Higher-order type of the collection to be built
   * @return
   */
  def to[C[_]](implicit builder: Builder[T @uv, C[T] @uv]): C[T @uv] = {
    val b = builder
    b ++= self
    b.result
  }


  /**
   * Builds a structure based on this traversable sequence given an implicit builder.
   * @param builder An implicit builder
   * @tparam S Type of the structure to be built
   * @return A new structure of type `R`
   */
  def build[S](implicit builder: Builder[T, S]): S = {
    val b = builder
    b ++= self
    b.result
  }

  def ++[U >: T](that: Traversable[U]) = this concat that

}

object Traversable {


  
}
