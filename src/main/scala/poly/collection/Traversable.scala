package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._
import poly.util.fastloop._
import poly.util.specgroup._
import poly.util.typeclass._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Basic trait for Poly-collection traversable collections.
 * @author Tongfei Chen (ctongfei@gmail.com).
 *
 * @define LAZY This function is lazily executed.
 * @define EAGER This function is eagerly executed.
 */
trait Traversable[+T] { self =>

  /**
   * Applies a function ''f'' to each element of this collection. $EAGER
   * @param f The function to be applied. Return values are discarded.
   * @tparam V Type of the result of function ''f''
   */
  def foreach[V](f: T => V): Unit

  /**
   * Returns a new traversable collection by applying a function to all elements in this collection.
   * $LAZY
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
   * $LAZY
   * @param f
   * @return
   */
  def filter(f: T => Boolean): Traversable[T] = new Traversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (f(x)) g(x)
    }
  }

  def filterNot(f: T => Boolean): Traversable[T] = filter(e => !f(e))

  /**
   * $EAGER
   * @param f
   * @return
   */
  def partition(f: T => Boolean): (Seq[T], Seq[T]) = {
    val l, r = ArraySeq.newBuilder[T]
    for (x ← self)
      if (f(x)) l += x else r += x
    (l.result, r.result)
  }

  /**
   * $EAGER
   * @param fs
   * @return
   */
  def filterMany(fs: (T => Boolean)*): Seq[Seq[T]] = {
    val l = ArraySeq.fill(fs.length)(ArraySeq[T]())
    for (x ← self)
      for (i ← 0 until fs.length opt)
        if (fs(i)(x)) l(i) append x
    l
  }

  def group[U >: T](implicit ev: Eq[U]): Set[Set[U]] = ???

  def groupBy[T1 >: T, K](f: T1 => K): Multimap[K, T1] = ???
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

  def takeWhile(f: T => Boolean): Traversable[T] = new Traversable[T] {
    def foreach[U](g: T => U): Unit = {
      for (x ← self) {
        if (!f(x)) return
        g(x)
      }
    }
  }

  def takeTo(f: T => Boolean): Traversable[T] = new Traversable[T] {
    def foreach[U](g: T => U): Unit = {
      var goal = false
      for (x ← self) {
        if (f(x)) goal = true
        g(x)
        if (goal) return
      }
    }
  }

  def takeUntil(f: T => Boolean): Traversable[T] = takeWhile(x => !f(x))

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

  def sort[X >: T](implicit O: WeakOrder[X]): IndexedSeq[X] = {
    val seq = self.map(e => e.asInstanceOf[X]).to[ArraySeq]
    seq.inplaceSort()(O)
    seq
  }

  def sortBy[U >: T, X](f: U => X)(implicit O: WeakOrder[X]): IndexedSortedSeq[U] = {
    val seq = self.to[ArraySeq]
    seq.inplaceSort()(WeakOrder by f)
    seq.asIfSorted(WeakOrder by f)
  }

  def sum[X >: T](implicit G: AdditiveMonoid[X]): X = reduce(G.add)

  def isum[X >: T](implicit G: InplaceAdditiveMonoid[X]) = {
    val sum = G.zero
    for (x ← self) G.inplaceAdd(sum, x)
    sum
  }

  def product[X >: T](implicit G: MultiplicativeSemigroup[X]): X = reduce(G.mul)

  def min[X >: T](implicit O: WeakOrder[X]): X = reduce(O.min[X])

  def max[X >: T](implicit O: WeakOrder[X]): X = reduce(O.max[X])

  def argmin[U: WeakOrder](f: T => U): T = argminWithValue(f)._1

  def argmax[U: WeakOrder](f: T => U): T = argmaxWithValue(f)._1

  def minAndMax[X >: T : WeakOrder]: (X, X) = {
    var minVal = default[X]
    var maxVal = default[X]
    var first = true

    for (x ← self) {
      if (first || minVal > x) {
        minVal = x
        first = false
      }
      if (first || maxVal < x) {
        maxVal = x
        first = false
      }
    }
    (minVal, maxVal)
  }

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

  def buildString[U >: T](delimiter: String)(implicit ev: Formatter[U]): String = {
    val sb = new StringBuilder
    var first = true
    for (x ← this) {
      if (first) {
        sb.append(ev.str(x))
        first = false
      } else {
        sb.append(delimiter)
        sb.append(ev.str(x))
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

  def ++[U >: T](that: Traversable[U]): Traversable[U] = this concat that

}

object Traversable {

  def empty: Traversable[Nothing] = new Traversable[Nothing] {
    def foreach[U](f: Nothing => U): Unit = {}
  }

  def single[T](e: T): Traversable[T] = new Traversable[T] {
    def foreach[U](f: T => U) = f(e)
  }

  implicit object Monad extends Monad[Traversable] {
    def flatMap[x, y](mx: Traversable[x])(f: x => Traversable[y]) = mx.flatMap(f)
    def id[x](u: x) = Traversable.single(u)
  }
  
}
