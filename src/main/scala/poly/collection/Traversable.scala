package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection.exception._
import poly.collection.mut._
import poly.util.fastloop._
import poly.util.typeclass._
import poly.util.typeclass.ops._

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Basic trait for traversable collections.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 *
 * @define LAZY This function is lazily executed.
 * @define EAGER This function is eagerly executed.
 * @define CX_NLOGN Time complexity: O(''n'' log ''n'').
 * @define CX_N Time complexity: O(''n'').
 * @define CX_LOGN Time complexity: O(log ''n'').
 * @define CX_AMORTIZED_1 Time complexity: Amortized O(1).
 * @define CX_1 Time complexity: O(1).
 */
trait Traversable[+T] { self =>

  /**
   * Applies a function ''f'' to each element of this collection. $EAGER $CX_N
   * @param f The function to be applied. Return values are discarded.
   * @tparam V Type of the result of function ''f''
   */
  def foreach[V](f: T => V): Unit

  /** Returns whether the size of this collection can be efficiently retrieved. $CX_1 */
  def hasKnownSize = false

  //region HELPER FUNCTIONS

  //region Monadic operations (map, flatMap, product)
  /**
   * Returns a new traversable collection by applying a function to all elements in this collection.
   * $LAZY $CX_1
   * @param f Function to apply
   * @tparam U Type of the image of the function
   * @return A new collection that each element is the image of the original element applied by ''f''.
   */
  def map[U](f: T => U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      for (x ← self) g(f(x))
    }
  }

  /** $LAZY $CX_1 */
  def flatMap[U](f: T => Traversable[U]): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V): Unit = {
      for (x ← self)
        for (y ← f(x))
          g(y)
    }
  }

  /** $LAZY $CX_1 */
  def cartesianProduct[U](that: Traversable[U]): Traversable[(T, U)] =
    self flatMap (x => that map (y => (x, y)))

  //endregion

  //region Filtering & grouping (count, filter, filterNot, filterMany, partition, groupBy)

  /**
   * Counts the number of elements that satisfy the specified predicate. $EAGER $CX_1
   * @param f The specified predicate
   * @return The number of elements that satisfy ''f''
   */
  def count(f: T => Boolean): Int = {
    var s = 0
    for (x ← self)
      if (f(x)) s += 1
    s
  }

  /**
   * Selects the elements that satisfy the specified predicate. $LAZY $CX_1
   * @param f The specified predicate
   * @return A traversable collection that contains all elements that satisfy ''f''.
   */
  def filter(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (f(x)) g(x)
    }
  }

  /**
   * Selects the elements that do not satisfy the specified predicate. $LAZY $CX_1
   * @param f The specified predicate
   * @return A traversable collection that contains all elements that do not satisfy ''f''.
   */
  def filterNot(f: T => Boolean): Traversable[T] = filter(e => !f(e))

  /**
   * Partitions this collection to two collections according to a predicate. $EAGER $CX_N
   * @param f The specified predicate
   * @return A pair of collections: ( {x|f(x)} , {x|!f(x)} )
   */
  def partition(f: T => Boolean): (Seq[T], Seq[T]) = {
    val l, r = ArraySeq.newBuilder[T]
    for (x ← self)
      if (f(x)) l += x else r += x
    (l.result, r.result)
  }

  /**
   * $EAGER $CX_N
   * @param fs
   * @return
   */
  def filterMany(fs: (T => Boolean)*): Seq[Seq[T]] = {
    val l = ArraySeq.fill(fs.length)(ArraySeq[T]())
    for (x ← self)
      for (i ← Range(fs.length))
        if (fs(i)(x)) l(i) appendInplace x
    l
  }

  /** $EAGER $CX_N */
  def findFirst(f: T => Boolean): Option[T] = {
    for (x ← self)
      if (f(x)) return Some(x)
    None
  }

  /** $EAGER $CX_N */
  def groupBy[S >: T, K](f: S => K): Multimap[K, S] = ???
  //endregion

  //region Concatenation (concat, prepend, append)
  /**
   * Concatenates two traversable collections into one. $LAZY $CX_1
   * @param that Another collection
   * @return A concatenated collection
   */
  def concat[U >: T](that: Traversable[U]): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V): Unit = {
      for (x ← self)
        f(x)
      for (x ← that)
        f(x)
    }
  }

  /** $LAZY $CX_1 */
  def prepend[U >: T](x: U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      f(x)
      self foreach f
    }
  }

  /** $LAZY $CX_1 */
  def append[U >: T](x: U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      self foreach f
      f(x)
    }
  }
  //endregion

  /**
   * Returns the number of elements in this collection. $EAGER $CX_N
   * @return The size of this collection
   */
  def size: Int = {
    var s = 0
    for (x ← self) s += 1
    s
  }

  /**
   * Checks if this collection is empty. $EAGER $CX_1
   * @return
   */
  def isEmpty = headOption match {
    case Some(e) => false
    case None => true
  }

  /** $EAGER $CX_N */
  def exists(f: T => Boolean): Boolean = {
    for (x ← self)
      if (f(x)) return true
    false
  }

  /** $EAGER $CX_N */
  def forall(f: T => Boolean): Boolean = {
    for (x ← self)
      if (!f(x)) return false
    true
  }

  /** $EAGER $CX_N */
  def foldLeft[U](z: U)(f: (U, T) => U): U = {
    var r = z
    for (x ← self)
      r = f(r, x)
    r
  }

  /** $EAGER $CX_N */
  def foldRight[U](z: U)(f: (T, U) => U): U = {
    reverse.foldLeft(z)((x, y) => f(y, x))
  }

  /** $EAGER $CX_N */
  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  /** $EAGER $CX_N */
  def reduce[U >: T](f: (U, U) => U): U = {
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

  /** $EAGER $CX_N */
  def reduceByMonoid[U >: T](m: Monoid[U]): U = {
    var res = m.id
    for (x ← self) res = m.op(res, x)
    res
  }

  /** $LAZY $CX_1 */
  def scanLeft[U >: T](z: U)(f: (U, T) => U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      var accum = z
      for (x ← self) {
        g(accum)
        accum = f(accum, x)
      }
      g(accum)
    }
  }

  /** $LAZY $CX_1 */
  def scan[U >: T](z: U)(f: (U, U) => U): Traversable[U] = scanLeft(z)(f)

  /** $LAZY $CX_1 */
  def scanByMonoid[U >: T](m: Monoid[U]): Traversable[U] = scanLeft(m.id)(m.op)

  /** $LAZY $CX_1 */
  def diff[U](f: (T, T) => U): Traversable[U] = new AbstractTraversable[U] {
    var first = true
    var prev: T = _
    def foreach[V](g: U => V) = {
      for (x ← self) {
        if (first) {
          prev = x
          first = false
        }
        else {
          g(f(x, prev))
          prev = x
        }
      }
    }
  }

  /** $LAZY $CX_1 */
  def diffByGroup[U >: T](g: Group[U]) = diff((x, y) => g.op(x, g.inv(y)))

  /** $EAGER $CX_1 */
  def head: T = {
    for (x ← self)
      return x
    throw new NoSuchElementException
  }

  /** $EAGER $CX_1 */
  def headOption: Option[T] = {
    for (x ← self)
      return Some(x)
    None
  }

  /** $LAZY $CX_1 */
  def tail: Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var first = true
      for (x ← self) {
        if (!first) f(x)
        first = false
      }
    }
  }

  /** $EAGER $CX_N */
  def last: T = {
    var p = head
    for (x ← this) p = x
    p
  }

  def init: Traversable[T] = new AbstractTraversable[T] {
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

  def take(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        f(x)
        i += 1
        if (i >= n) return
      }
    }
  }

  def skip(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        if (i >= n) f(x)
        i += 1
      }
    }
  }

  def takeWhile(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](g: T => U): Unit = {
      for (x ← self) {
        if (!f(x)) return
        g(x)
      }
    }
  }

  def takeTo(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
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

  def skipWhile(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](g: T => U): Unit = {
      var starts = false
      for (x ← self) {
        if (!starts && !f(x)) starts = true
        if (starts) g(x)
      }
    }
  }

  def slice(i: Int, j: Int) = skip(i).take(j - i)

  def distinct: Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      val set = HashSet[T]()
      for (x ← self) {
        if (!set.contains(x)) {
          set.add(x)
          f(x)
        }
      }
    }
  }

  def reverse: BiSeq[T] = self.to[ArraySeq].reverse

  def sort[X >: T](implicit O: WeakOrder[X]): IndexedSeq[X] = {
    val seq = self.map(_.asInstanceOf[X]).to[ArraySeq]
    seq.sortInplace()(O)
    seq
  }

  def sortBy[U >: T, X](f: U => X)(implicit O: WeakOrder[X]): IndexedSortedSeq[U] = {
    val seq = self.to[ArraySeq]
    seq.sortInplace()(WeakOrder by f)
    seq.asIfSorted(WeakOrder by f)
  }

  def sum[X >: T : AdditiveMonoid]: X = fold(zero)(_+_)

  def isum[X >: T](implicit G: InplaceAdditiveCMonoid[X]) = {
    val sum = zero[X]
    for (x ← self) G.inplaceAdd(sum, x)
    sum
  }

  def prefixSums[X >: T](implicit G: AdditiveMonoid[X]) = scan(zero)(G.add)

  def differences[X >: T](implicit G: AdditiveGroup[X]) = diff(G.sub)

  def product[X >: T](implicit G: MultiplicativeMonoid[X]): X = fold(G.one)(G.mul)

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

  //region Building (to, buildString)
  /**
   * Converts this traversable sequence to any collection type.
   * @param builder An implicit builder
   * @tparam C Higher-order type of the collection to be built
   * @return
   */
  def to[C[_]](implicit builder: Builder[T @uv, C[T] @uv]): C[T @uv] = {
    val b = builder
    if (hasKnownSize) b.sizeHint(size)
    b ++= self
    b.result
  }

  /**
   * Builds a structure based on this traversable sequence given an implicit builder.
   * @param builder An implicit builder
   * @tparam S Type of the structure to be built
   * @return A new structure of type `S`
   */
  def build[S](implicit builder: Builder[T, S]): S = {
    val b = builder
    if (hasKnownSize) b.sizeHint(size)
    b ++= self
    b.result
  }

  def buildString[U >: T : Formatter](delimiter: String): String = {
    val sb = new StringBuilder
    var first = true
    for (x ← this) {
      if (first) {
        sb.append(x.str)
        first = false
      } else {
        sb.append(delimiter)
        sb.append(x.str)
      }
    }
    sb.result()
  }
  //endregion

  //region Symbolic aliases
  def :+[U >: T](x: U) = this append x
  def +:[U >: T](x: U) = this prepend x
  def ++[U >: T](that: Traversable[U]) = this concat that
  def ×[U](that: Traversable[U]) = this cartesianProduct that
  def |>[U](f: T => U) = this map f
  def |?(f: T => Boolean) = this filter f
  def ||>[U](f: T => Traversable[U]) = this flatMap f
  def |&[U >: T](f: (U, U) => U) = this reduce f
  //endregion

  //endregion

  override def toString = "(" + buildString(",") + ")"
}

object Traversable {

  object empty extends Traversable[Nothing] {
    def foreach[U](f: Nothing => U): Unit = {}
  }

  def single[T](e: T): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U) = f(e)
  }

  implicit object Monad extends Monad[Traversable] {
    def flatMap[X, Y](mx: Traversable[X])(f: X => Traversable[Y]) = mx.flatMap(f)
    def id[X](u: X) = Traversable.single(u)
  }
  
}

abstract class AbstractTraversable[+T] extends Traversable[T]

