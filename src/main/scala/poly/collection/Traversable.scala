package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.mut._
import poly.macroutil._
import poly.util.typeclass._
import poly.util.typeclass.ops._

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.reflect._

/**
 * Represents a collection whose elements can be traversed through.
 * @author Tongfei Chen
 * @since 0.1.0
 * @define LAZY The resulting collection is '''lazily''' computed.
 * @define EAGER The resulting collection is '''eagerly''' computed.
 * @define Onlogn Time complexity: O(n log n).
 * @define On Time complexity: O(n).
 * @define Ologn Time complexity: O(log n).
 * @define O1amortized Time complexity: Amortized O(1).
 * @define O1 Time complexity: O(1).
 */
trait Traversable[+T] { self =>

  /**
   * Applies a specific function to each element in this collection.
   * @param f The function to be applied. Return values are discarded.
   */
  def foreach[V](f: T => V): Unit

  // HELPER FUNCTIONS

  /**
   * Returns a new collection by applying a function to all elements in this collection. $LAZY
   * @example {{{(1, 2, 3) map { _ + 1 } == (2, 3, 4)}}}
   */
  def map[U](f: T => U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      for (x ← self) g(f(x))
    }
  }

  /**
    * Builds a new collection by applying a function to all elements of this collection
    * and using the elements of the resulting collections.
    * $LAZY This is the direct equivalent of the Haskell function `bind`/`>>=`.
    * @example {{{(0, 1, 2, 3) flatMap { i => i repeat i } == (1, 2, 2, 3, 3, 3)}}}
    */
  def flatMap[U](f: T => Traversable[U]): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](g: U => V): Unit = {
      for (x ← self)
        for (y ← f(x))
          g(y)
    }
  }

  /**
    * Returns the Cartesian product of two traversable sequences. $LAZY
    * @example {{{(1, 2) cartesianProduct (1, 2) == ((1, 1), (1, 2), (2, 1), (2, 2))}}}
    * @param that Another traversable sequence
    */
  def cartesianProduct[U](that: Traversable[U]): Traversable[(T, U)] =
    self flatMap (x => that map (y => (x, y)))

  /**
   * Counts the number of elements in this collection that satisfy the specified predicate.
   * @return The number of elements that satisfy ''f''
   */
  def count(f: T => Boolean): Int = {
    var s = 0
    for (x ← self)
      if (f(x)) s += 1
    s
  }

  /**
   * Selects only the elements that satisfy the specified predicate. $LAZY
   * @example {{{(1, 2, 3, 4) filter { _ > 2 } == (3, 4)}}}
   * @return A traversable collection that contains all elements that satisfy ''f''.
   */
  def filter(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](g: T => V) = {
      for (x ← self)
        if (f(x)) g(x)
    }
  }

  /**
   * Selects the elements that do not satisfy the specified predicate. $LAZY
   * @return A traversable collection that contains all elements that do not satisfy ''f''.
   */
  def filterNot(f: T => Boolean): Traversable[T] = filter(x => !f(x))

  /**
   * Partitions this collection to two collections according to a predicate. $EAGER
   * @return A pair of collections: ( {x|f(x)} , {x|!f(x)} )
   */
  def partition(f: T => Boolean): (Seq[T], Seq[T]) = {
    val l, r = ArraySeq.newBuilder[T]
    for (x ← self)
      if (f(x)) l add x else r add x
    (l.result, r.result)
  }

  /**
   * Puts each element in this collection into multiple bins, where each bin is specified by a predicate. $EAGER
   * @param fs
   * @return
   */
  def filterMany(fs: (T => Boolean)*): IndexedSeq[Seq[T]] = {
    val l = ArraySeq.fill(fs.length)(ArraySeq[T]())
    for (x ← self)
      FastLoop.ascending(0, fs.length, 1) { i =>
        if (fs(i)(x)) l(i) appendInplace x
      }
    l
  }

  /**
    * Finds the first element in this collection that satisfy the given predicate. If not found, [[None]].
    */
  def findFirst(f: T => Boolean): Option[T] = {
    for (x ← self)
      if (f(x)) return Some(x)
    None
  }

  /** $EAGER $On */
  def groupBy[K: IntHashing](f: T => K): Map[K, Traversable[T]] = {
    val m = HashMap[K, ArraySeq[T]]()
    for (x ← self)
      m(f(x)) appendInplace x
    m
  }
  //endregion

  //region Concatenation (concat, prepend, append)
  /**
   * Concatenates two traversable collections into one. $LAZY
   * @example {{{(1, 2, 3) ++ (4, 5) == (1, 2, 3, 4, 5)}}}
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

  /** Prepends an element to the beginning of this collection. $LAZY */
  def prepend[U >: T](x: U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      f(x)
      self foreach f
    }
  }

  /** Appends an element to the end of this collection. $LAZY */
  def append[U >: T](x: U): Traversable[U] = new AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      self foreach f
      f(x)
    }
  }
  //endregion

  /**
   * Returns the number of elements in this collection. $On
   * @return The size of this collection
   */
  def size: Int = {
    var s = 0
    for (x ← self) s += 1
    s
  }

  /** Checks if this collection is empty. $O1 */
  def isEmpty = headOption match {
    case Some(e) => false
    case None => true
  }

  /** $On */
  def exists(f: T => Boolean): Boolean = {
    for (x ← self)
      if (f(x)) return true
    false
  }

  /** $On */
  def forall(f: T => Boolean): Boolean = {
    for (x ← self)
      if (!f(x)) return false
    true
  }

  /** $On */
  def foldLeft[U](z: U)(f: (U, T) => U): U = {
    var r = z
    for (x ← self)
      r = f(r, x)
    r
  }

  /** $On */
  def foldRight[U](z: U)(f: (T, U) => U): U = reverse.foldLeft(z)((s, t) => f(t, s))

  /** $On */
  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  /** $On */
  def foldByMonoid[U >: T : Monoid]: U = foldLeft(id)(_ op _)

  /** $On */
  def reduceLeft[U >: T](f: (U, T) => U): U = {
    var empty = true
    var res = default[U]
    for (x ← self) {
      if (empty) {
        res = x
        empty = false
      }
      else res = f(res, x)
    }
    if (empty) throw new EmptyCollectionReductionException
    res
  }

  def reduceRight[U >: T](f: (T, U) => U): U = self.reverse.reduceLeft[U]((u, t) => f(t, u))

  def reduce[U >: T](f: (U, U) => U) = reduceLeft(f)

  def reduceBySemigroup[U >: T : Semigroup]: U = reduceLeft[U](_ op _)

  /** $LAZY $O1 */
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

  /** $LAZY $O1 */
  def scan[U >: T](z: U)(f: (U, U) => U): Traversable[U] = scanLeft(z)(f)

  /** $LAZY $O1 */
  def scanByMonoid[U >: T : Monoid]: Traversable[U] = scanLeft(id)(_ op _)

  /**
   * Returns the consecutive differences of the sequences. $LAZY
   * @example {{{ (0, 1, 3, 6, 10).consecutive(_ - _) == (1, 2, 3, 4) }}}
   */
  def consecutive[U](f: (T, T) => U): Traversable[U] = new AbstractTraversable[U] {
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

  /** $LAZY $O1 */
  def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) => U.op(x, U.inv(y)))

  /** $EAGER $O1 */
  def head: T = {
    for (x ← self)
      return x
    throw new DummyNodeException
  }

  /** $EAGER $O1 */
  def headOption: Option[T] = {
    for (x ← self)
      return Some(x)
    None
  }

  /** $LAZY $O1 */
  def tail: Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var first = true
      for (x ← self) {
        if (!first) f(x)
        first = false
      }
    }
  }

  /** $EAGER $On */
  def last: T = {
    var p = head
    for (x ← this) p = x
    p
  }

  def init: Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var p = default[T]
      var first = true
      for (x ← self) {
        if (first) first = false
        else f(p)
        p = x
      }
    }
  }

  def suffixes = Iterable.iterate(self)(_.tail).takeUntil(_.isEmpty)

  def prefixes: Iterable[Iterable[T]] = to[ArraySeq].prefixes

  def take(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        f(x)
        i += 1
        if (i >= n) return
      }
    }
    override def take(nn: Int) = self.take(math.min(n, nn))
  }

  def skip(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x ← self) {
        if (i >= n) f(x)
        i += 1
      }
    }
    override def skip(nn: Int) = self.skip(n + nn)
  }

  def takeWhile(f: T => Boolean): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](g: T => U): Unit = {
      for (x ← self) {
        if (f(x)) g(x)
        else return
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

  /** Returns the unique elements in this collection while retaining its original order. */
  def distinct[U >: T : IntHashing]: Traversable[U] = new AbstractTraversable[U] {
    private[this] val set = HashSet[U]()
    def foreach[V](f: U => V) = {
      for (x ← self) {
        if (set notContains x) {
          set add x
          f(x)
        }
      }
    }
  }

  def distinctBy[U: IntHashing](f: T => U): Traversable[T] = new AbstractTraversable[T] {
    private[this] val set = HashSet[U]()
    def foreach[V](g: T => V) = {
      for (x ← self) {
        val fx = f(x)
        if (set notContains fx) {
          set add fx
          g(x)
        }
      }
    }
  }

  def union[U >: T : IntHashing](that: Traversable[U]): Traversable[U] = (this concat that).distinct

  def intersect[U >: T : IntHashing](that: Traversable[U]): Traversable[U] = (this filter that.to[HashSet]).distinct

  /** Returns the reverse of this collection. $EAGER */
  def reverse: BiSeq[T] = self.to[ArraySeq].reverse

  /** Returns a randomly shuffled version of this collection. $EAGER */
  def shuffle: IndexedSeq[T] = {
    val a = self.to[ArraySeq]
    a.shuffleInplace()
    a
  }

  /**
    * Rotates this collection from the index specified. $LAZY
    * @example {{{(1, 2, 3, 4).rotate(1) == (2, 3, 4, 1)}}}
    * @param n Rotation starts here
    */
  def rotate(n: Int) = (self skip n) ++ (self take n)

  /**
   * Sorts this collection in ascending order using the implicitly provided order. $EAGER
   * @example {{{
   *   (3, 2, 4, 1).sort == (1, 2, 3, 4)
   *   (3, 2, 4, 1).sort(WeakOrder[Int].reverse) == (4, 3, 2, 1)
   * }}}
   */
  def sort[U >: T](implicit U: WeakOrder[U]): SortedIndexedSeq[T @uv] = {
    val seq = self.to[ArraySeq]
    seq.sortInplace()(U)
    seq.asIfSorted(U)
  }

  def sortBy[U](f: T => U)(implicit U: WeakOrder[U]): SortedIndexedSeq[T @uv] = {
    val seq = self.to[ArraySeq]
    val o = WeakOrder by f
    seq.sortInplace()(o) // TODO: cache Us and sort to avoid recalculation of Us?
    seq.asIfSorted(o)
  }

  /**
   * Repeats this collection for a specific number of times. $LAZY
   * @example {{{(1, 2, 3).repeat(2) == (1, 2, 3, 1, 2, 3)}}}
   * @param n Number of times to repeat
   */
  def repeat(n: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      FastLoop.ascending(0, n, 1) { i =>
        for (x ← self) f(x)
      }
    }
  }

  /**
    * Infinitely cycles through this collection. $LAZY
    * @example {{{(1, 2, 3).cycle == (1, 2, 3, 1, 2, 3, 1, 2...)}}}
    */
  def cycle: Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      while (true) for (x ← self) f(x)
    }
  }

  /**
   * Returns the sum of the elements in this collection.
   * @example {{{(1, 2, 3).sum == 6}}}
   * @tparam U Supertype of the type of elements: must be endowed with an additive monoid.
   * @return The sum
   */
  def sum[U >: T : AdditiveCMonoid]: U = fold(zero)(_+_)

  def sumBy[U: AdditiveCMonoid](f: T => U) = map(f).sum

  def sumInplace[U >: T](implicit U: InplaceAdditiveCMonoid[U]) = {
    val sum = zero[U]
    for (x ← self) U.addInplace(sum, x)
    sum
  }

  /**
   * Returns the prefix sums of this collection.
   * @example {{{(1, 2, 3, 4).prefixSums == (0, 1, 3, 6, 10)}}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive monoid
   * @return The prefix sums sequence
   */
  def prefixSums[X >: T](implicit X: AdditiveMonoid[X]) = scan(zero)(_+_)

  /**
   * Returns the consecutive differences sequence of this collection.
   * @example {{{
   *   (0, 1, 3, 6, 10).differences == (1, 2, 3, 4)
   * }}}
   * @note This is the inverse operator of `prefixSums` that the following invariant holds:
   * {{{
   *   l.prefixSums.differences == l
   * }}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive group (to enable the `sub(-)` operation).
   * @return The consecutive differences sequence
   */
  def differences[X >: T](implicit X: AdditiveGroup[X]) = consecutive(X.sub)

  /**
   * Returns the minimum element in this collection.
   * @return The minimum element
   */
  def min(implicit T: WeakOrder[T]): T = reduce(T.min[T])

  def max(implicit T: WeakOrder[T]): T = reduce(T.max[T])

  /**
    * Returns the first element in this collection that makes the specific function least.
    * @param f The function
    * @tparam U The implicit weak order on the output of the specific function.
    */
  def argmin[U: WeakOrder](f: T => U): T = argminWithValue(f)._1

  def minBy[U: WeakOrder](f: T => U) = argmin(f)

  /**
    * Returns the first element in this collection that makes the specific function greatest.
    * @param f
    * @tparam U
    * @return
    */
  def argmax[U: WeakOrder](f: T => U): T = argmaxWithValue(f)._1

  def maxBy[U: WeakOrder](f: T => U) = argmax(f)

  def minAndMax(implicit T: WeakOrder[T]): (T, T) = {
    var minVal = default[T]
    var maxVal = default[T]
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
   */
  def to[C[_]](implicit builder: Builder[T @uv, C[T] @uv]): C[T @uv] = build(builder)

  /**
   * Converts this traversable sequence to any collection type given a factory.
   * @param factory A collection factory
   * @tparam C Higher-order type of the collection to be built
   */
  def to[C[_]](factory: Factory[C]): C[T @uv] = to(factory.newBuilder[T])

  /**
   * Converts this traversable sequence to an array.
   */
  def toArray[U >: T : ClassTag]: Array[U] = {
    val n = self.size
    val a = Array.ofDim[U](n)
    var i = 0
    for (x ← self) {
      a(i) = x
      i += 1
    }
    a
  }

  /**
   * Builds a structure based on this traversable sequence given an implicit builder.
   * @param builder An implicit builder
   * @tparam S Type of the structure to be built
   * @return A new structure of type `S`
   */
  def build[S](implicit builder: Builder[T, S]): S = {
    val b = builder
    if (self.isInstanceOf[HasKnownSize]) b.sizeHint(size)
    b addAll self
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
  def /:[U](z: U)(f: (U, T) => U) = foldLeft(z)(f)
  def :\[U](z: U)(f: (T, U) => U) = foldRight(z)(f)
  def :+[U >: T](x: U) = this append x
  def +:[U >: T](x: U) = this prepend x
  def ++[U >: T](that: Traversable[U]) = this concat that
  def |×|[U](that: Traversable[U]) = this cartesianProduct that

  def |>[U](f: T => U) = this map f
  def |?(f: T => Boolean) = this filter f
  def ||>[U](f: T => Traversable[U]) = this flatMap f
  //endregion
  //endregion

  def asTraversable: Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = self.foreach(f)
  }

  override def toString = "(" + buildString(", ") + ")"
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

  implicit class TraversableOfTraversablesOps[T](val underlying: Traversable[Traversable[T]]) extends AnyVal {
    /**
      * "Flattens" this collection of collection into one collection.
      * @example {{{((1, 2, 3), (), (7)).flatten == (1, 2, 3, 7)}}}
      */
    def flatten: Traversable[T] = underlying.flatMap(identity)
  }

  implicit class TraversableOfPairsOps[A, B](val underlying: Traversable[(A, B)]) extends AnyVal {

    /**
      * Lazily unzips a traversable sequence of pairs.
      * @example {{{((1, 'a'), (2, 'b'), (3, 'c')).unzip == ((1, 2, 3), ('a', 'b', 'c'))}}}
      */
    def unzip: (Traversable[A], Traversable[B]) = (underlying map firstOfPair, underlying map secondOfPair)

    /**
      * Eagerly unzips a traversable sequence of pairs. This method only traverses through the collection once.
      */
    def unzipEagerly: (IndexedSeq[A], IndexedSeq[B]) = {
      val ak = ArraySeq.newBuilder[A]
      val av = ArraySeq.newBuilder[B]
      for ((k, v) ← underlying) {
        ak add k
        av add v
      }
      (ak.result, av.result)
    }

    /**
      * Converts this traversable sequence to a map if this sequence consists of (key, value) pairs.
      * @param builder Implicit builder of the map
      */
    def toMap[M[_, _]](implicit builder: Builder[(A, B), M[A, B]]) = underlying.build(builder)

  }

}

abstract class AbstractTraversable[+T] extends Traversable[T]
