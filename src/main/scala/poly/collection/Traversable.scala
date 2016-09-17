package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.syntax._
import poly.algebra.mut._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.mut._
import poly.macroutil._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.reflect._

/**
 * Represents a collection whose elements can be traversed through.
 * @author Tongfei Chen
 * @since 0.1.0
 * @define LAZY '''[LAZY]'''
 * @define EAGER '''[EAGER]'''
 * @define Onlogn '''O(''n'' log ''n'')'''
 * @define On '''O(''n'')'''
 * @define Ologn '''O(log ''n'')'''
 * @define O1amortized '''Amortized O(1)'''
 * @define O1 '''O(1)'''
 */
trait Traversable[+T] { self =>

  /**
   * Applies a specific function to each element in this collection.
   * @param f The function to be applied. Return values are discarded.
   */
  def foreach[V](f: T => V): Unit

  // HELPER FUNCTIONS

  /** Returns if the size of this collection can be efficiently retrieved. */
  def sizeKnown: Boolean = false

  /**
   * Returns a new collection by applying a function to all elements in this collection. $LAZY
   * @example {{{(1, 2, 3) map { _ + 1 } == (2, 3, 4)}}}
   */
  def map[U](f: T => U): Traversable[U] = new TraversableT.Mapped(self, f)

  /**
   * Builds a new collection by applying a function to all elements of this collection
   * and using the elements of the resulting collections.
   * $LAZY This is the direct equivalent of the Haskell function `bind`/`>>=`.
   * @example {{{(0, 1, 2, 3) flatMap { i => i repeat i } == (1, 2, 2, 3, 3, 3)}}}
   */
  def flatMap[U](f: T => Traversable[U]): Traversable[U] = new TraversableT.FlatMapped(self, f)

  /**
   * Returns the monadic product of two traversable sequences. $LAZY
   * @example {{{(1, 2) monadicProduct (1, 2) == ((1, 1), (1, 2), (2, 1), (2, 2))}}}
   */
  def monadicProduct[U](that: Traversable[U]): Traversable[(T, U)] =
    for (x <- self; y <- that) yield (x, y)

  /**
   * Selects only the elements that satisfy the specified predicate. $LAZY
   * @example {{{(1, 2, 3, 4) filter { _ > 2 } == (3, 4)}}}
   */
  def filter(f: T => Boolean): Traversable[T] = new TraversableT.Filtered(self, f)

  def collect[U](pf: PartialFunction[T, U]): Traversable[U] = new TraversableT.Collected(self, pf)

  /** Tests if this collection contains the given element. $On */
  def contains[U >: T : Eq](u: U): Boolean = {
    for (x <- self)
      if (u === x) return true
    false
  }

  /** Selects the elements that do not satisfy the specified predicate. $LAZY */
  def filterNot(f: T => Boolean): Traversable[T] = filter(!f)

  /**
   * Partitions this collection to two collections according to a predicate. $EAGER
   * @return A pair of collections: ( {x|f(x)} , {x|!f(x)} )
   */
  def partition(f: T => Boolean): (Iterable[T], Iterable[T]) = {
    val l, r = ArraySeq.newBuilder[T]
    for (x <- self)
      if (f(x)) l += x else r += x
    (l.result, r.result)
  }

  /** Puts each element in this collection into multiple bins, where each bin is specified by a predicate. $EAGER
   * @example {{{
   *   (0, 1, 2, 3, 4, 5).filterMany(_ % 2 == 1, _ % 3 == 1)
   *   == ((1, 3, 5), (1, 4))
   * }}}
   */
  def filterMany(fs: (T => Boolean)*): IndexedSeq[Iterable[T]] = {
    val l = ArraySeq.fill(fs.length)(ArraySeq[T]())
    for (x <- self)
      FastLoop.ascending(0, fs.length, 1) { i =>
        if (fs(i)(x)) l(i) appendInplace x
      }
    l
  }

  /** Finds the first element in this collection that satisfy the given predicate. If not found, [[None]]. */
  def findFirst(f: T => Boolean): Option[T] = {
    for (x <- self)
      if (f(x)) return Some(x)
    None
  }

  def group[U >: T : Eq]: Map[U, Iterable[T]] = groupBy(identity)

  /** $EAGER $On */
  def groupBy[K: Eq](f: T => K): Map[K, Iterable[T]] = {
    val m = AutoMap[K, ArraySeq[T]]().withDefaultUpdate(ArraySeq[T]())
    for (x <- self)
      m(f(x)) :+= x
    m
  }

  //endregion

  /**
   * Concatenates two traversable collections into one. $LAZY
   * @example {{{(1, 2, 3) ++ (4, 5) == (1, 2, 3, 4, 5)}}}
   */
  def concat[U >: T](that: Traversable[U]): Traversable[U] = new TraversableT.Concatenated(self, that)

  /** Prepends an element to the beginning of this collection. $LAZY */
  def prepend[U >: T](x: U): Traversable[U] = new TraversableT.Prepended(self, x)

  /** Appends an element to the end of this collection. $LAZY */
  def append[U >: T](x: U): Traversable[U] = new TraversableT.Appended(self, x)

  /** Counts the number of elements in this collection that satisfy the specified predicate. $On */
  def count(f: T => Boolean): Int = {
    var s = 0
    for (x <- self)
      if (f(x)) s += 1
    s
  }

  /**
   * Returns the number of elements in this collection. $On
   * @note May not terminate if this collection is infinite.
   */
  def size: Int = {
    var s = 0
    for (x <- self) s += 1
    s
  }

  /** Checks if this collection is empty. $O1 */
  def isEmpty = headOption match {
    case Some(e) => false
    case None => true
  }

  final def notEmpty = !isEmpty

  /** Checks if the given predicate holds for at least one element in this collection. $On */
  def exists(f: T => Boolean): Boolean = {
    for (x <- self)
      if (f(x)) return true
    false
  }

  /** Checks if the given predicate holds for all elements in this collection. $On */
  def forall(f: T => Boolean): Boolean = {
    for (x <- self)
      if (!f(x)) return false
    true
  }

  /** $On */
  def foldLeft[U](z: U)(f: (U, T) => U): U = {
    var r = z
    for (x <- self)
      r = f(r, x)
    r
  }

  def foldLeftByAction[U](z: U)(f: Action[U, T]) = foldLeft(z)(f.act)

  /** $On */
  def foldRight[U](z: U)(f: (T, U) => U): U = reverse.foldLeft(z)((s, t) => f(t, s))

  def foldRightByAction[U](z: U)(f: Action[U, T]) = foldRight(z)((s, t) => f.act(t, s))

  /** $On */
  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  /** $On */
  def foldByMonoid[U >: T : Monoid]: U = foldLeft(id)(_ <> _)

  /** $On */
  def reduceLeft[U >: T](f: (U, T) => U): U = { //TODO: Action[U, T]
    var empty = true
    var res = default[U]
    for (x <- self) {
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

  def reduceBySemigroup[U >: T : Semigroup]: U = reduceLeft[U](_ <> _)

  /** $LAZY $O1 */
  def scanLeft[U](z: U)(f: (U, T) => U): Traversable[U] = new TraversableT.Scanned(self, z, f)

  def scanRight[U](z: U)(f: (T, U) => U) = self.reverse.scanLeft(z)((x, y) => f(y, x)).reverse

  /** $LAZY $O1 */
  def scan[U >: T](z: U)(f: (U, U) => U): Traversable[U] = scanLeft(z)(f)

  /** $LAZY $O1 */
  def scanByMonoid[U >: T : Monoid]: Traversable[U] = scanLeft(id)(_ <> _)

  /**
   * Returns the consecutive differences of the sequences. $LAZY
   * @example {{{ (0, 1, 3, 6, 10).consecutive(_ - _) == (1, 2, 3, 4) }}}
   */
  def consecutive[U](f: (T, T) => U): Traversable[U] = new TraversableT.Consecutive(self, f)

  /** $LAZY $O1 */
  def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) => U.op(x, U.inv(y)))

  /** $EAGER $O1 */
  def head: T = {
    for (x <- self)
      return x
    throw new DummyNodeException
  }

  /** $EAGER $O1 */
  def headOption: Option[T] = {
    for (x <- self)
      return Some(x)
    None
  }

  /** $LAZY $O1 */
  def tail: Traversable[T] = new TraversableT.Tail(self)

  /** $EAGER $On */
  def last: T = {
    var l = head
    for (x <- this) l = x
    l
  }

  def init: Traversable[T] = new TraversableT.Init(self)

  def suffixes: Iterable[Iterable[T]] = to(ArraySeq).suffixes

  def prefixes: Iterable[Iterable[T]] = to(ArraySeq).prefixes

  def take(n: Int): Traversable[T] = new TraversableT.Taken(self, n)

  def drop(n: Int): Traversable[T] = new TraversableT.Dropped(self, n)

  def takeWhile(f: T => Boolean): Traversable[T] = new TraversableT.TakenWhile(self, f)

  def takeTo(f: T => Boolean): Traversable[T] = new TraversableT.TakenTo(self, f)

  def takeUntil(f: T => Boolean): Traversable[T] = takeWhile(!f)

  def dropWhile(f: T => Boolean): Traversable[T] = new TraversableT.DroppedWhile(self, f)

  def dropUntil(f: T => Boolean): Traversable[T] = dropWhile(!f)

  def slice(i: Int, j: Int) = drop(i).take(j - i)

  /**
   * Returns the unique elements in this collection while retaining its original order.
   * This function requires that an equivalence relation is endowed on the type of the elements.
   *
   * @example {{{
   *   (1, 4, 1, 3, 4, 2).distinct == (1, 4, 3, 2)
   * }}}
   */
  def distinct[U >: T : Eq]: Traversable[U] = distinctBy(identity)

  def distinctBy[U: Eq](f: T => U): Traversable[T] = new TraversableT.DistinctBy(self, f, Eq[U])

  def union[U >: T : Eq](that: Traversable[U]): Traversable[U] =
    (this concat that).distinct

  def intersect[U >: T : Eq](that: Traversable[U]): Traversable[U] =
    (this filter that.to(AutoSet)).distinct


  /** Returns the reverse of this collection. $EAGER */
  def reverse: BidiIterable[T] = self.to(ArraySeq).reverse

  /** Returns a randomly shuffled version of this collection. $EAGER */
  def shuffle: IndexedSeq[T] = {
    val a = self.to(ArraySeq)
    a.shuffleInplace()
    a
  }

  /**
   * Rotates this collection from the index specified. $LAZY
   *
   * @example {{{(1, 2, 3, 4).rotate(1) == (2, 3, 4, 1)}}}
   * @param n Rotation starts here
   */
  def rotate(n: Int) = (self drop n) ++ (self take n)

  /**
   * Sorts this collection in ascending order using the implicitly provided order. $EAGER
   * @example {{{
   *   (3, 2, 4, 1).sort == (1, 2, 3, 4)
   *   (3, 2, 4, 1).sort(Order[Int].reverse) == (4, 3, 2, 1)
   * }}}
   */
  def sort(implicit T: Order[T]): SortedIndexedSeq[T @uv] = {
    val seq = self to ArraySeq
    seq.sortInplace()(T)
    seq.asIfSorted(T)
  }

  def sortBy[U: Order](f: T => U): SortedIndexedSeq[T @uv] = {
    val seq = self to ArraySeq
    val w = ArraySeq.tabulate(seq.length)(i => f(seq(i))) // cache the weights of each term!
    seq sortInplaceUsing w
    seq asIfSorted (Order by f)
  }

  /**
   * Pairs each element in this collection with an index while traversing.
   * @example {{{ (a, b, c).withIndex == ((0, a), (1, b), (2, c)) }}}
   */
  def withIndex: Traversable[(Int, T)] = new TraversableT.WithIndex(self)

  /**
   * Repeats this collection for a specific number of times. $LAZY
   * @example {{{ (1, 2, 3) repeat 2 == (1, 2, 3, 1, 2, 3) }}}
   */
  def repeat(n: Int): Traversable[T] = new TraversableT.Repeated(self, n)

  /**
   * Infinitely cycles through this collection. $LAZY
   * @example {{{(1, 2, 3).cycle == (1, 2, 3, 1, 2, 3, 1, 2, ...)}}}
   */
  def cycle: Traversable[T] = new TraversableT.Cycled(self)

  /**
   * Returns the sum of the elements in this collection.
   * @example {{{(1, 2, 3).sum == 6}}}
   * @tparam U Supertype of the type of elements: must be endowed with an additive monoid.
   * @return The sum
   */
  def sum[U >: T : AdditiveCMonoid]: U = fold(zero[U])(_+_)

  def sumBy[U: AdditiveCMonoid](f: T => U) = map(f).sum

  def sumInplace[U >: T](implicit U: InplaceAdditiveMonoid[U]) = {
    val sum = U.zero
    for (x <- self) U.addInplace(sum, x)
    sum
  }

  /**
   * Returns the prefix sums of this collection.
   * @example {{{(1, 2, 3, 4).prefixSums == (0, 1, 3, 6, 10)}}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive monoid
   * @return The prefix sums sequence
   */
  def prefixSums[X >: T](implicit X: AdditiveMonoid[X]) = scan(X.zero)(X.add)

  /**
   * Returns the consecutive differences sequence of this collection.
   *
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

  /** Returns the minimum element in this collection. */
  def min(implicit T: Order[T]): T = reduce(T.min[T])

  /** Returns the maximum element in this collection. */
  def max(implicit T: Order[T]): T = reduce(T.max[T])

  /**
   * Returns the top-''k'' elements in this collection.
   */
  def top(k: Int)(implicit T: Order[T]) = {
    val beam = Beam.ofWidth[T](k)(T.reverse)
    self foreach beam.push
    beam.elements
  }

  def topBy[U: Order](f: T => U)(k: Int) = {
    val beam = Beam.ofWidth(k)((Order by second[T, U]).reverse)
    for (x <- self) beam.push(x -> f(x))
    beam.elements map first
  }

  /**
   * Returns the first element in this collection that makes the specific function least.
   * @example {{{
   *   (1, 2, 3, 4, 5) argmin { _ % 4 } == 4
   * }}}
   */
  def argmin[U: Order](f: T => U): T = argminWithValue(f)._1

  def minBy[U: Order](f: T => U) = argmin(f)

  /**
   * Returns the first element in this collection that makes the specific function greatest.
   * @example {{{
   *   (1, 2, 3, 4, 5) argmax { _ % 4 } == 3
   * }}}
   */
  def argmax[U: Order](f: T => U): T = argmaxWithValue(f)._1

  def maxBy[U: Order](f: T => U) = argmax(f)

  def minAndMax(implicit T: Order[T]): (T, T) = {
    var minVal = default[T]
    var maxVal = default[T]
    var first = true

    for (x <- self) {
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

  def argminWithValue[U: Order](f: T => U): (T, U) = {
    var minKey = default[T]
    var minVal = default[U]
    var first = true

    for (x <- self) {
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

  def argmaxWithValue[U: Order](f: T => U): (T, U) = {
    var maxKey = default[T]
    var maxVal = default[U]
    var first = true

    for (x <- self) {
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
   * Converts this traversable sequence to any collection type given a factory.
   * @example {{{ xs to ArraySeq }}}
   */
  def to[U >: T, C[_]](factory: FactoryA[C]): C[U] = factory from self

  /**
   * Converts this traversable sequence to any collection type given a factory that requires an additional evidence.
   * @example {{{ xs to HashSet }}}
   */
  def to[U >: T : Ev, C[_], Ev[_]](factory: FactoryA_EvA[C, Ev]): C[U] = factory from self

  // Seems not useful, type signature too complicated
  //def to[U >: T : EvU, V: EvV, C[_, _], EvU[_], EvV[_]](factory: FactoryEv2[C, EvU, EvV]): C[U, V] = build(factory.newBuilder[U, V])

  /**
   * Converts this traversable sequence to an array. $EAGER
   * @example {{{ xs.toArray }}}
   */
  def toArray[U >: T : ClassTag]: Array[U] = {
    val n = self.size
    val a = Array.ofDim[U](n)
    var i = 0
    for (x <- self) {
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
    if (self.sizeKnown) b.sizeHint(self.size)
    b addAllInplace self
    b.result
  }

  def buildString(delimiter: String): String = {
    val sb = new StringBuilder
    var first = true
    for (x <- this) {
      if (first) {
        sb.append(x.toString)
        first = false
      } else {
        sb.append(delimiter)
        sb.append(x.toString)
      }
    }
    sb.result()
  }
  //endregion

  def asIfSizeKnown(s: Int): Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = self.foreach(f)
    override def sizeKnown = true
    override def size = s
  }

  //region Symbolic aliases
  def /:[U](z: U)(f: (U, T) => U) = foldLeft(z)(f)
  def :\[U](z: U)(f: (T, U) => U) = foldRight(z)(f)
  def :+[U >: T](x: U) = this append x
  def +:[U >: T](x: U) = this prepend x
  def ++[U >: T](that: Traversable[U]) = this concat that
  def |*|[U](that: Traversable[U]) = this monadicProduct that
  def |>[U](f: T => U) = this map f

  //endregion
  //endregion

  def asTraversable: Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = self.foreach(f)
  }

  def withFilter(f: T => Boolean) = filter(f)

  private[poly] def toString0 = {
    val sb = new StringBuilder
    sb.append(self.take(Settings.MaxElementsToString).buildString(", "))
    if (self.drop(Settings.MaxElementsToString).notEmpty) sb.append(", ⋯")
    sb.result()
  }

  override def toString = s"($toString0)"
  // hashCode/equals: by reference

}

object Traversable {

  // CONSTRUCTORS

  object empty extends Traversable[Nothing] {
    def foreach[U](f: Nothing => U): Unit = {}
  }

  def single[T](e: T): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U) = f(e)
  }

  // TYPECLASS INSTANCES

  implicit object Monad extends ConcatenativeMonad[Traversable] {
    def flatMap[X, Y](mx: Traversable[X])(f: X => Traversable[Y]) = mx.flatMap(f)
    def id[X](u: X) = Traversable.single(u)
    def empty[X]: Traversable[X] = Traversable.empty
    def concat[X](sx: Traversable[X], sy: Traversable[X]): Traversable[X] = sx ++ sy
  }

  // IMPLICIT CONVERSIONS

  implicit class TraversableOfTraversablesOps[T](val underlying: Traversable[Traversable[T]]) extends AnyVal {
    /**
     * "Flattens" this collection of collection into one collection.
     * @example {{{((1, 2, 3), (), (7)).flatten == (1, 2, 3, 7)}}}
     */
    def flatten: Traversable[T] = underlying.flatMap(x => x)
  }

  implicit class TraversableOfPairsOps[A, B](val underlying: Traversable[(A, B)]) extends AnyVal {

    /**
     * Lazily unzips a traversable sequence of pairs.
     * @example {{{((1, 'a'), (2, 'b'), (3, 'c')).unzip == ((1, 2, 3), ('a', 'b', 'c'))}}}
     */
    def unzip: (Traversable[A], Traversable[B]) = (underlying map first, underlying map second)

    /** Eagerly unzips a traversable sequence of pairs. This method only traverses through the collection once. */
    def unzipE: (IndexedSeq[A], IndexedSeq[B]) = {
      val ak = ArraySeq.newBuilder[A]
      val av = ArraySeq.newBuilder[B]
      for ((k, v) <- underlying) {
        ak addInplace k
        av addInplace v
      }
      (ak.result, av.result)
    }

    def to[M[_, _]](factory: FactoryAB[M]) = factory from underlying

    def to[M[_, _], Ev[_]](factory: FactoryAB_EvA[M, Ev])(implicit A: Ev[A]) = factory from underlying

    def to[M[_, _], EvA[_], EvB[_]](factory: FactoryAB_EvAB[M, EvA, EvB])(implicit A: EvA[A], B: EvB[B]) = factory from underlying

  }

}

abstract class AbstractTraversable[+T] extends Traversable[T]

private[poly] object TraversableT {

  class Mapped[T, U](self: Traversable[T], f: T => U) extends AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      for (x <- self) g(f(x))
    }
    override def sizeKnown = self.sizeKnown // map preserves size
    override def size = self.size
  }

  class FlatMapped[T, U](self: Traversable[T], f: T => Traversable[U]) extends AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      for (x <- self; y <- f(x)) g(y)
    }
  }

  class Filtered[T](self: Traversable[T], f: T => Boolean) extends AbstractTraversable[T] {
    def foreach[V](g: T => V) = {
      for (x <- self) if (f(x)) g(x)
    }
  }

  class Collected[T, U](self: Traversable[T], pf: PartialFunction[T, U]) extends AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      for (x <- self) (pf runWith f)(x)
    }
  }

  class Concatenated[T](self: Traversable[T], that: Traversable[T]) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      for (x <- self) f(x)
      for (x <- that) f(x)
    }
  }

  class Prepended[T](self: Traversable[T], x: T) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      f(x)
      for (x <- self) f(x)
    }
  }

  class Appended[T](self: Traversable[T], x: T) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      for (x <- self) f(x)
      f(x)
    }
  }

  class Scanned[T, U](self: Traversable[T], z: U, f: (U, T) => U) extends AbstractTraversable[U] {
    def foreach[V](g: U => V) = {
      var accum = z
      for (x <- self) {
        g(accum)
        accum = f(accum, x)
      }
      g(accum)
    }
  }

  class Consecutive[T, U](self: Traversable[T], f: (T, T) => U) extends AbstractTraversable[U] {
    var first = true
    var prev: T = _
    def foreach[V](g: U => V) = {
      for (x <- self) {
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

  class Tail[T](self: Traversable[T]) extends AbstractTraversable[T] {
    def foreach[U](f: T => U) = {
      var first = true
      for (x <- self) {
        if (!first) f(x)
        first = false
      }
    }
  }

  class Init[T](self: Traversable[T]) extends AbstractTraversable[T] {
    def foreach[U](f: T => U) = {
      var p = default[T]
      var first = true
      for (x <- self) {
        if (first) first = false
        else f(p)
        p = x
      }
    }
  }

  class Taken[T](self: Traversable[T], n: Int) extends AbstractTraversable[T] {
    def foreach[U](f: T => U): Unit = {
      var i = 0
      for (x <- self) {
        f(x)
        i += 1
        if (i >= n) return
      }
    }
    override def take(nn: Int) = self.take(Math.min(n, nn))
    //TODO: override sizeKnown/size?
  }

  class Dropped[T](self: Traversable[T], n: Int) extends AbstractTraversable[T] {
    def foreach[U](f: T => U) = {
      var i = 0
      for (x <- self) {
        if (i >= n) f(x)
        i += 1
      }
    }
    override def drop(nn: Int) = self.drop(n + nn)
  }

  class TakenWhile[T](self: Traversable[T], f: T => Boolean) extends AbstractTraversable[T] {
    def foreach[U](g: T => U): Unit = {
      for (x <- self) {
        if (f(x)) g(x)
        else return
      }
    }
  }

  class TakenTo[T](self: Traversable[T], f: T => Boolean) extends AbstractTraversable[T] {
    def foreach[U](g: T => U): Unit = {
      var goal = false
      for (x <- self) {
        if (f(x)) goal = true
        g(x)
        if (goal) return
      }
    }
  }

  class DroppedWhile[T](self: Traversable[T], f: T => Boolean) extends AbstractTraversable[T] {
    def foreach[U](g: T => U) = {
      var starts = false
      for (x <- self) {
        if (!starts && !f(x)) starts = true
        if (starts) g(x)
      }
    }
  }

  class DistinctBy[T, U](self: Traversable[T], f: T => U, e: Eq[U]) extends AbstractTraversable[T] {
    private[this] val set = AutoSet[U]()(e)
    def foreach[V](g: T => V) = {
      for (x <- self) {
        val u = f(x)
        if (set notContains u) {
          set += u
          g(x)
        }
      }
    }
  }

  class WithIndex[T](self: Traversable[T]) extends AbstractTraversable[(Int, T)] {
    def foreach[V](f: ((Int, T)) => V) = {
      var i = 0
      for (x <- self) {
        f(i, x)
        i += 1
      }
    }
  }

  class Repeated[T](self: Traversable[T], n: Int) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      FastLoop.ascending(0, n, 1) { i =>
        for (x <- self) f(x)
      }
    }
  }

  class Cycled[T](self: Traversable[T]) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      while (true) for (x <- self) f(x)
    }
  }

}
