package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection.exception._
import poly.collection.mut._
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
   * Returns a new collection by applying a function to all elements in this collection.
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
  def filterMany(fs: (T => Boolean)*): IndexedSeq[Seq[T]] = {
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
  def foldRight[U](z: U)(f: (T, U) => U): U = reverse.foldLeft(z)((u, t) => f(t, u))

  /** $EAGER $CX_N */
  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  /** $EAGER $CX_N */
  def foldByMonoid[U >: T : Monoid]: U = foldLeft(id)(_ op _)
  
  /** $EAGER $CX_N */
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
    if (first) throw new EmptyCollectionReductionException
    res
  }

  def reduceRight[U >: T](f: (T, U) => U): U = self.reverse.reduceLeft[U]((u, t) => f(t, u))

  def reduce[U >: T](f: (U, U) => U) = reduceLeft(f)

  def reduceBySemigroup[U >: T : Semigroup]: U = reduceLeft[U](_ op _)

  /** $LAZY $CX_1 */
  def scanLeft[U](z: U)(f: (U, T) => U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      var accum = z
      for (x ← self) {
        g(accum)
        accum = f(accum, x)
      }
      g(accum)
    }
  }

  def scanRight[U](z: U)(f: (T, U) => U) = self.reverse.scanLeft(z)((x, y) => f(y, x))

  /** $LAZY $CX_1 */
  def scan[U >: T](z: U)(f: (U, U) => U): Traversable[U] = scanLeft(z)(f)

  /** $LAZY $CX_1 */
  def scanByMonoid[U >: T : Monoid]: Traversable[U] = scanLeft(id)(_ op _)

  /**
   * Returns the consecutive differences of the sentences. E.g. {{{
   *   (0, 1, 3, 6, 10, 15).diff(_ - _) == (1, 2, 3, 4, 5)
   * }}}.
   * $LAZY $CX_1
   */
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
  def diffByGroup[U >: T](implicit U: Group[U]) = diff((x, y) => U.op(x, U.inv(y)))

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

  def rotate(n: Int): Traversable[T] = self.skip(n) ++ self.take(n)

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

  /**
   * Returns the unique elements in this collection while retaining its original order.
   */
  //TODO: should be [U >: T](implicit U: Equiv[U]): Traversable[U], but postponed to later versions
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

  //TODO: should be [U >: T](implicit U: Equiv[U]): Traversable[U], but postponed to later versions
  def union[U >: T](that: Traversable[U]): Traversable[U] = (this concat that).distinct

  def intersect[U >: T](that: Traversable[U]): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V): Unit = {
      val set = this.to[HashSet]
      for (x ← that)
        if (set contains x) f(x)
    }
  }

  def reverse: BiSeq[T] = self.to[ArraySeq].reverse

  def sort[U >: T](implicit U: WeakOrder[U]): SortedIndexedSeq[U] = {
    val seq = self.map(_.asInstanceOf[U]).to[ArraySeq]
    seq.sortInplace()(U)
    seq.asIfSorted(U)
  }

  def sortBy[U >: T, X](f: U => X)(implicit X: WeakOrder[X]): SortedIndexedSeq[U] = {
    val seq = self.to[ArraySeq]
    val o = WeakOrder by f
    seq.sortInplace()(o)
    seq.asIfSorted(o)
  }

  /**
   * Repeats this collection ''n'' times. For example, `(1, 2, 3).repeat(2)` becomes `(1, 2, 3, 1, 2, 3)`.
   * @param n Number of times to repeat
   */
  def repeat(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      for (i ← Range(n))
        for (x ← self) f(x)
    }
  }

  /**
   * Returns the sum of the elements in this collection. For example, `(1, 2, 3).sum` returns `6`.
   * @tparam X Supertype of the type of elements: must be endowed with an additive monoid.
   * @return The sum
   */
  def sum[X >: T : AdditiveMonoid]: X = fold(zero)(_+_)

  def isum[X >: T](implicit X: InplaceAdditiveCMonoid[X]) = {
    val sum = zero[X]
    for (x ← self) X.inplaceAdd(sum, x)
    sum
  }

  /**
   * Returns the prefix sums of this collection. For example, `(1, 2, 3, 4).prefixSums` becomes `(0, 1, 3, 6, 10)`.
   * @tparam X Supertype of the type of elements: must be endowed with an additive monoid
   * @return The prefix sums sequence
   */
  def prefixSums[X >: T](implicit X: AdditiveMonoid[X]) = scan(zero)(_+_)

  /**
   * Returns the consecutive differences sequence of this collection. For example, `(0, 1, 3, 6, 10).differences`
   * becomes `(1, 2, 3, 4)`. This is the inverse operator of `prefixSums` that the following invariant holds:
   * {{{
   *   l.prefixSums.differences == l
   * }}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive group (to enable the `sub(-)` operation).
   * @return The consecutive differences sequence
   */
  def differences[X >: T](implicit X: AdditiveGroup[X]) = diff(X.sub)

  /**
   * Returns the product of the elements in this collection. For example, `(1, 2, 3, 4, 5).product` returns `120`.
   * @tparam X Supertype of the type of elements: must be endowed with a multiplicative monoid.
   * @return The product
   */
  def product[X >: T](implicit X: MultiplicativeMonoid[X]): X = fold(one)(_*_)

  /**
   * Returns the minimum element in this collection.
   * @tparam X Supertype of the type of elements: must be endowed with a weak order.
   * @return The minimum element
   */
  def min[X >: T](implicit X: WeakOrder[X]): X = reduceLeft(X.min[X])

  def max[X >: T](implicit X: WeakOrder[X]): X = reduceLeft(X.max[X])

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
    if (first) throw new EmptyCollectionReductionException
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
    if (first) throw new EmptyCollectionReductionException
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
    if (first) throw new EmptyCollectionReductionException
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
  def |+[U >: T](f: (U, U) => U) = this reduce f
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

