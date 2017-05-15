package poly.collection

import cats.implicits._
import algebra.instances.int._
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
 * @define Onlogn '''O(''n'' log ''n''):'''
 * @define On '''O(''n''):'''
 * @define Ologn '''O(log ''n''):'''
 * @define O1amortized '''Amortized O(1):'''
 * @define O1 '''O(1):'''
 */
trait Traversable[+T] { self =>

  import Traversable._
  
  /**
   * $On Applies a specific callback function to each element in this collection.
   * @param f The function to be applied. Return values are discarded.
   */
  def foreach[V](f: T => V): Unit

  //region SIZE OPS

  /** $O1 Checks whether the size of this collection can be efficiently retrieved. */
  def sizeKnown: Boolean = false

  /**
   * $On Returns the number of elements in this collection.
   * @note May not terminate if this collection is infinite.
   * If ( `this.sizeKnown` ), the time complexity is O(1).
   */
  def size: Int = {
    var s = 0
    for (_ <- self) s += 1
    s
  }

  /** $O1 Checks if this collection is empty. */
  def isEmpty = headOption match {
    case Some(_) => false
    case None    => true
  }

  /** $O1 Checks if this collection is not empty. */
  final def notEmpty = !isEmpty

  //endregion

  //region MONADIC OPS

  /**
   * $LAZY Returns a new collection by applying a function to all elements in this collection.
   * @example {{{(1, 2, 3) map { _ + 1 } == (2, 3, 4)}}}
   */
  def map[U](f: T => U): Traversable[U] = new Mapped(self, f)

  /**
   * $LAZY Builds a new collection by applying a function to all elements of this collection
   * and using the elements of the resulting collections.
   * This is the direct equivalent of the Haskell function `bind`/`>>=`.
   * @example {{{(0, 1, 2, 3) flatMap { i => i repeat i } == (1, 2, 2, 3, 3, 3)}}}
   */
  def flatMap[U](f: T => Traversable[U]): Traversable[U] = new FlatMapped(self, f)

  /**
   * $LAZY Returns the monadic product of two traversable collections, i.e., a collection
   * that contains all possible pairs in which the first element comes from the
   * first collection and the second element from the second collection.
   * @example {{{ (1, 2) product (1, 2) == ((1, 1), (1, 2), (2, 1), (2, 2)) }}}
   */
  def product[U](that: Traversable[U]): Traversable[(T, U)] =
    for (x <- self; y <- that) yield (x, y)

  /**
   * $LAZY Returns the monadic product of two traversable collections, in which each pair
   * is transformed using the specific binary function.
   * @example {{{ (1, 2) productWith (1, 2) {_+_} == (2, 3, 3, 4) }}}
   */
  def productWith[U, X](that: Traversable[U])(f: (T, U) => X): Traversable[X] =
    for (x <- self; y <- that) yield f(x, y)

  /**
   * Zips this traversable collection with an [[Iterable]].
   */
  def zip[U](that: Iterable[U]): Traversable[(T, U)] = new ZippedWith(self, that, (t: T, u: U) => (t, u))

  def zipWith[U, X](that: Iterable[U])(f: (T, U) => X): Traversable[X] = new ZippedWith(self, that, f)
  //endregion

  //region FILTERING OPS
  /**
   * $LAZY Selects only the elements that satisfy the specified predicate.
   * @example {{{(1, 2, 3, 4) filter { _ > 2 } == (3, 4)}}}
   */
  def filter(f: T => Boolean): Traversable[T] = new Filtered(self, f)

  /** $LAZY Selects the elements that do not satisfy the specified predicate. */
  def filterNot(f: T => Boolean): Traversable[T] = filter(!f)

  /**
   * $LAZY Returns a collection by applying the specified partial function to
   * all elements of this collection on which the function is defined.
   */
  def collect[U](pf: PartialFunction[T, U]): Traversable[U] = new Collected(self, pf)

  /**
   * $LAZY Returns a collection by applying the specific function to all elements,
   * and then collect those whose result is not `None`.
   */
  def collectOption[U](f: T => Option[U]): Traversable[U] = new OptionallyCollected(self, f)

  /** $On Counts the number of elements in this collection that satisfy the specified predicate. */
  def count(f: T => Boolean): Int = {
    var s = 0
    for (x <- self)
      if (f(x)) s += 1
    s
  }

  /**
   * $EAGER $On Partitions this collection to two collections according to a predicate.
   * @return A pair of collections: ( {x|f(x)} , {x|!f(x)} )
   */
  def partition(f: T => Boolean): (Iterable[T], Iterable[T]) = {
    val l, r = ArraySeq.newBuilder[T]
    for (x <- self)
      if (f(x)) l << x else r << x
    (l.result, r.result)
  }

  /** $EAGER $On Puts each element in this collection into multiple bins, where each bin is specified by a predicate.
   * @example {{{
   *   (0, 1, 2, 3, 4, 5).filterMany(_ % 2 == 1, _ % 3 == 1)
   *   == ((1, 3, 5), (1, 4))
   * }}}
   */
  def filterMany(fs: (T => Boolean)*): IndexedSeq[Iterable[T]] = {
    val l = ArraySeq.fill(fs.length)(ArraySeq[T]())
    for (x <- self)
      FastLoop.ascending(0, fs.length, 1) { i =>
        if (fs(i)(x)) l(i) :+= x
      }
    l
  }

  /** $EAGER $On Finds the first element in this collection that satisfy the given predicate. If not found, [[None]]. */
  def findFirst(f: T => Boolean): Option[T] = {
    for (x <- self)
      if (f(x)) return Some(x)
    None
  }
  //endregion

  //region SET OPS

  /**
   * $EAGER $On Groups all elements in this collection by an implicit equivalence relation.
   */
  def group[U >: T : Eq]: Map[U, Iterable[T]] = groupBy(identity)

  /**
   * $EAGER $On Groups all elements in this collection by a key selector.
   */
  def groupBy[K: Eq](f: T => K): Map[K, Iterable[T]] = {
    val m = AutoMap[K, ArraySeq[T]]().withDefaultUpdate(ArraySeq[T]())
    for (x <- self)
      m(f(x)) :+= x
    m
  }

  /** $On Tests if this collection contains the given element. */
  def contains[U >: T : Eq](u: U): Boolean = {
    for (x <- self)
      if (u === x) return true
    false
  }

  /**
   * $LAZY Returns the unique elements in this collection while retaining its original order.
   * This function requires that an equivalence relation is endowed on the type of the elements.
   * @note The performance when being iterated depends on the equivalence relation: <ul>
   *           <li> [[Hashing]]: amortized O(''n''); </li>
   *           <li> [[Order]]: O(''n'' log ''n''); </li>
   *           <li> otherwise: O(''n''^2^).
   *         </ul>
   * @example {{{
   *   (1, 4, 1, 3, 4, 2).distinct == (1, 4, 3, 2)
   * }}}
   */
  def distinct[U >: T](implicit U: Eq[U]): Traversable[U] = distinctBy[U](identity)

  def distinctBy[U: Eq](f: T => U): Traversable[T] = new DistinctBy(self, f, Eq[U])

  def union[U >: T : Eq](that: Traversable[U]): Traversable[U] =
    (this concat that).distinct

  def intersect[U >: T : Eq](that: Traversable[U]): Traversable[U] =
    if (this.sizeKnown && that.sizeKnown && this.size < that.size) // short circuit!
      (that filter (this: Traversable[U]).to(AutoSet)).distinct
    else (this filter that.to(AutoSet)).distinct[U]

  /**
   * $On Returns a weighted set (multiset) of all the elements in this collection.
   * @example {{{ (a, b, c, a, b).occCounts == {a: 2, b: 2, c: 1} }}}
   */
  def occCounts[U >: T : Eq]: WeightedSet[U, Int] = {
    import algebra.instances.int._
    PairWeightedSet.of[Int] from self
  }
  //endregion

  //region SEQ OPS
  /**
   * $LAZY Concatenates two traversable collections into one.
   * @example {{{(1, 2, 3) ++ (4, 5) == (1, 2, 3, 4, 5)}}}
   */
  def concat[U >: T](that: Traversable[U]): Traversable[U] = new Concatenated(self, that)

  /** $LAZY Prepends an element to the beginning of this collection. */
  def prepend[U >: T](x: U): Traversable[U] = new Prepended(self, x)

  /** $LAZY Appends an element to the end of this collection. */
  def append[U >: T](x: U): Traversable[U] = new Appended(self, x)

  /** $EAGER $O1 Returns the first element of this collection. */
  def head: T = {
    for (x <- self)
      return x
    throw new DummyNodeException
  }

  /** $LAZY Returns the collection without the first element. */
  def tail: Traversable[T] = new Tail(self)

  /** $EAGER $On Returns the last element of this collection. */
  def last: T = {
    var l = head
    for (x <- this) l = x
    l
  }

  /** $LAZY Returns the collection without the last element. */
  def init: Traversable[T] = new Init(self)

  /** $EAGER $O1 */
  def headOption: Option[T] = {
    for (x <- self)
      return Some(x)
    None
  }

  /** $EAGER $On */
  def lastOption: Option[T] = {
    if (isEmpty) None
    else Some(last)
  }

  /**
   * Returns the list of tails (suffixes) of this collection.
   * @example {{{(1, 2, 3).suffixes == ((1, 2, 3), (2, 3), (3))}}}
   */
  def suffixes: Iterable[Iterable[T]] = to(ArraySeq).suffixes

  /**
   * Returns the list of inits (prefixes) of this collection.
   * @example {{{(1, 2, 3).prefixes == ((1, 2, 3), (1, 2), (1))}}}
   */
  def prefixes: Iterable[Iterable[T]] = to(ArraySeq).prefixes

  def take(n: Int): Traversable[T] = new Taken(self, n)

  def takeWhile(f: T => Boolean): Traversable[T] = new TakenWhile(self, f)

  def takeTo(f: T => Boolean): Traversable[T] = new TakenTo(self, f)

  def takeUntil(f: T => Boolean): Traversable[T] = takeWhile(!f)

  def drop(n: Int): Traversable[T] = new Dropped(self, n)

  def dropWhile(f: T => Boolean): Traversable[T] = new DroppedWhile(self, f)

  def dropTo(f: T => Boolean): Traversable[T] = new DroppedTo(self, f)

  def dropUntil(f: T => Boolean): Traversable[T] = dropWhile(!f)

  def slice(i: Int, j: Int) = drop(i).take(j - i)

  /**
   * Pairs each element in this collection with an index while traversing.
   * @example {{{ (a, b, c).withIndex == ((0, a), (1, b), (2, c)) }}}
   */
  def withIndex: Traversable[(Int, T)] = new WithIndex(self)

  /**
   * Repeats this collection for a specific number of times. $LAZY
   * @example {{{ (1, 2, 3) repeat 2 == (1, 2, 3, 1, 2, 3) }}}
   */
  def repeat(n: Int): Traversable[T] = new Repeated(self, n)

  /**
   * Infinitely cycles through this collection. $LAZY
   * @example {{{(1, 2, 3).cycle == (1, 2, 3, 1, 2, 3, 1, 2, ...)}}}
   */
  def cycle: Traversable[T] = new Cycled(self)

  /**
   * Intersperses this collection with the given element.
   * @example {{{(1, 2, 3) intersperse 0 == (1, 0, 2, 0, 3)}}}
   */
  def intersperse[U >: T](u: U): Traversable[U] = new Interspersed(self, u)

  //endregion

  //region FOLDING/SCANNING OPS
  /**
   * $EAGER $On Applies a binary operator to a start value and all elements in this collection,
   * going from left to right.
   * @example {{{
   *   (1, 2, 3).foldLeft("")((s, x) => s"$s$x") == "123"
   * }}}
   * @param z Start value
   * @param f Binary operator
   */
  def foldLeft[U](z: U)(f: (U, T) => U): U = {
    var r = z
    for (x <- self)
      r = f(r, x)
    r
  }

  /**
   * $EAGER $On Applies a binary operator to a start value and all elements in this collection,
   * going to right to left.
   * @example {{{
   *   (3, 2, 1).foldRight(0)(_+_) == 6
   * }}}
   * */
  def foldRight[U](z: U)(f: (T, U) => U): U = reverse.foldLeft(z)((s, t) => f(t, s))

  /**
   * $EAGER $On Folds this collection using the given associative binary operator.
   * The folding order is undefined.
   */
  def fold[U >: T](z: U)(f: (U, U) => U): U = foldLeft(z)(f)

  /**
   * $EAGER $On Folds this collection using the identity and operator in the given monoid
   * that is endowed on a supertype of the type of the elements.
   */
  def foldByMonoid[U >: T](implicit U: Monoid[U]): U = foldLeft(U.empty)(U.combine)

  def reduceLeft[U >: T](f: (U, T) => U): U = {
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

  def reduceBySemigroup[U >: T : Semigroup]: U = reduceLeft[U](_ |+| _)

  /** $LAZY
   * Performs a prefix scan (left-to-right) of this collection.
   * @note The behavior is different from the Scala standard library: The starting element [[z]] is not returned.
   * @example {{{
   *   (1, 2, 3, 4).scanLeft(0)(_+_) == (1, 3, 6, 10)
   * }}}
   */
  def scanLeft[U](z: U)(f: (U, T) => U): Traversable[U] = new Scanned(self, z, f)

  /** $EAGER
   * Performs a suffix scan (right-to-left) of this collection.
   * @note The behavior is different from the Scala standard library: The starting element [[z]] is not returned.
   * @example {{{
   *   (1, 2, 3, 4).scanRight(0)(_+_) == (10, 9, 7, 4)
   * }}}
   */
  def scanRight[U](z: U)(f: (T, U) => U) = self.reverse.scanLeft(z)((x, y) => f(y, x)).reverse

  /** $LAZY $O1 */
  def scan[U >: T](z: U)(f: (U, U) => U): Traversable[U] = scanLeft(z)(f)

  /** $LAZY $O1 */
  def scanByMonoid[U >: T](implicit U: Monoid[U]): Traversable[U] = scanLeft(U.empty)(U.combine)

  /** $LAZY $O1 */
  def diffByGroup[U >: T](implicit U: Group[U]) = slidingPairsWith(U.remove)

  /** $On Checks if the given predicate holds for all elements in this collection. */
  def forall(f: T => Boolean): Boolean = {
    for (x <- self)
      if (!f(x)) return false
    true
  }

  /** $On Checks if the given predicate holds for at least one element in this collection. */
  def exists(f: T => Boolean): Boolean = {
    for (x <- self)
      if (f(x)) return true
    false
  }

  /**
   * Returns the sum of the elements in this collection.
   * @example {{{(1, 2, 3).sum == 6}}}
   * @tparam U Supertype of the type of elements: must be endowed with an additive monoid.
   * @return The sum
   */
  def sum[U >: T](implicit U: AdditiveMonoid[U]): U = fold(U.zero)(U.plus)

  def sumBy[U: AdditiveMonoid](f: T => U) = map(f).sum

  /**
   * Returns the prefix sums of this collection.
   * @example {{{(1, 2, 3, 4).prefixSums == (1, 3, 6, 10)}}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive monoid
   * @return The prefix sums sequence
   */
  def prefixSums[X >: T](implicit X: AdditiveMonoid[X]) = scan(X.zero)(X.plus)

  /**
   * Returns the consecutive differences sequence of this collection.
   *
   * @example {{{
   *   (1, 3, 6, 10).differences == (1, 2, 3, 4)
   * }}}
   * @note The following invariant should hold: {{{
   *   xs.prefixSums.differences == xs
   * }}}
   * @tparam X Supertype of the type of elements: must be endowed with an additive group (to enable the `sub(-)` operation).
   * @return The consecutive differences sequence
   */
  def differences[X >: T](implicit X: AdditiveGroup[X]) = slidingPairsWith((x, y) => X.minus(y, x)).prepend(head)

  /** $EAGER $On Returns the minimum element in this collection. */
  def min[U >: T](implicit U: Order[U]): T = reduce(U.refine[T].min)

  /** $EAGER $On Returns the maximum element in this collection. */
  def max[U >: T](implicit U: Order[U]): T = reduce(U.refine[T].max)

  /**
   * $EAGER '''O(''n'' log ''k''): '''
   * Returns the top-''k'' elements in this collection.
   * @note The internal implementation is a beam.
   */
  def top[U >: T](k: Int)(implicit U: Order[U]) = {
    val beam = Beam.ofSize[T](k)(U.refine[T].reverse)
    beam ++= self
    beam.elements
  }

  /** $EAGER $On
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

  def topBy[U: Order](f: T => U)(k: Int) = {
    val beam = Beam.ofSize(k)((Order by second[T, U]).reverse)
    for (x <- self) beam.enqueue(x -> f(x))
    beam.elements map first
  }

  /** $EAGER $On
   * Returns the minimum and the maximum element in this collection in one loop.
   */
  def minMax[U >: T](implicit U: Order[U]): (T, T) = {
    implicit val T = U.refine[T]
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

  //endregion

  //region SEQUENTIAL GROUPING OPS
  /**
   * $LAZY
   * @example {{{ (0, 1, 3, 6, 10).slidingPairsWith((x, y) => y - x) == (1, 2, 3, 4) }}}
   */
  def slidingPairsWith[U](f: (T, T) => U): Traversable[U] = new Consecutive(self, f)

  def slidingPairs: Traversable[(T, T)] = slidingPairsWith((x, y) => (x, y))

  /**
   * Groups elements in fixed size blocks by passing a sliding window over them.
   * @note Each block is of fixed size: this is different from the Scala standard library
   * where `Seq(1).sliding(2) == ((1))`. The behavior of this function is similar to the
   * standard library's `iterator.sliding(n).withPartial(false)`.
   * @param windowSize The size of the sliding window
   * @param step Step size. The default value is 1.
   * @example {{{
   *   (1, 2, 3, 4).sliding(2) == ((1, 2), (2, 3), (3, 4))
   *   (1).sliding(2) == (())
   * }}}
   */
  def sliding(windowSize: Int, step: Int = 1): Traversable[IndexedSeq[T]] = new Sliding(self, windowSize, step)

  def chunk(chunkSize: Int): Traversable[IndexedSeq[T]] = new Chunked(self, chunkSize)

  //TODO: groupConsecutively, split, splitBy
  //endregion

  //region REORDERING OPS
  /** $EAGER $On Reverses this collection. */
  def reverse: BidiIterable[T] = self.to(ArraySeq).reverse

  /** $EAGER $On Returns a randomly shuffled version of this collection. */
  def shuffle: IndexedSeq[T] = {
    val a = self to ArraySeq
    a.shuffle_!()
    a
  }

  /**
   * $LAZY Rotates this collection from the index specified.
   * @example {{{(1, 2, 3, 4).rotate(1) == (2, 3, 4, 1)}}}
   * @param n Rotation starts here
   */
  def rotate(n: Int) = (self drop n) concat (self take n)

  /**
   * $EAGER $Onlogn Sorts this collection in ascending order using the implicitly provided order.
   * @example {{{
   *   (3, 2, 4, 1).sort == (1, 2, 3, 4)
   *   (3, 2, 4, 1).sort(Order[Int].reverse) == (4, 3, 2, 1)
   * }}}
   */
  def sort[U >: T](implicit U: Order[U]): SortedIndexedSeq[U] = {
    val seq = self to ArraySeq
    seq.sort_![U]()(U)
    seq.asIfSorted(U)
  }

  /**
   * $EAGER $Onlogn Sorts this collection in ascending order by the value results from the given function.
   * @example {{{
   *   ("abc", "mn", "z").sortBy(_.length) == ("z", "mn", "abc")
   * }}}
   */
  def sortBy[U: Order](f: T => U): SortedIndexedSeq[T @uv] = {
    val seq = self to ArraySeq
    val w = ArraySeq.tabulate(seq.length)(i => f(seq(i))) // cache the weights of each term (just O(n) computations, instead of worst case O(n^2) computations of weights)!
    seq sortUsing_! w
    seq asIfSorted (Order by f)
  }
  //endregion

  //region BUILDING OPS

  /** $EAGER $On
   * Pipes the elements in this collection to a builder.
   */
  def >>>[R](builder: Builder[T, R]): Unit = builder <<< self

  /** $EAGER $On
   * Pipes the elements in this collection to a builder, then close the builder and return the built result.
   */
  def >>>![R](builder: Builder[T, R]) = {
    builder <<< self
    builder.result()
  }

  /**
   * Converts this traversable collection to any collection type.
   * @param factory Grounded factory (may require evidences)
   * @example {{{
   *   Seq(1, 2, 4, 8).to[ListSeq[Int]]
   *   Seq(1, 2, 4, 8).to[BitSet]
   * }}}
   */
  def to[R](implicit factory: Factory0[T, R]) = factory from self

  /**
   * Converts this traversable collection to any collection type given a factory that requires an additional evidence.
   * @example {{{ xs to HashSet }}}
   */
  def to[U >: T : Ev, C[_], Ev[_]](factory: Factory1[Id, C, Ev]): C[U] = factory from self

  /**
   * Converts this traversable sequence to an array. The data are always copied. $EAGER
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

  //region DECORATION OPS

  def asIfSizeKnown(s: Int): Traversable[T] = {
    if (self.sizeKnown) self
    else
      new AbstractTraversable[T] {
        def foreach[V](f: T => V) = self foreach f
        override def sizeKnown = true
        override def size = s
      }
  }
  //endregion

  //region SYMBOLIC ALIASES

  def /:[U](z: U)(f: (U, T) => U) = foldLeft(z)(f)
  def :\[U](z: U)(f: (T, U) => U) = foldRight(z)(f)
  def :+[U >: T](x: U) = this append x
  def +:[U >: T](x: U) = this prepend x
  def ++[U >: T](that: Traversable[U]) = this concat that

  //endregion

  //region CASTING

  def asTraversable: Traversable[T] = new AbstractTraversable[T] {
    def foreach[V](f: T => V) = self.foreach(f)
  }
  //endregion

  //region JAVA/SCALA CONFORMING METHODS

  private[poly] def toString0 = {
    val sb = new StringBuilder
    sb.append(self.take(Settings.MaxElementsToString).buildString(", "))
    if (self.drop(Settings.MaxElementsToString).notEmpty) sb.append(", ⋯")
    sb.result()
  }

  override def toString = s"($toString0)"
  // hashCode/equals: by reference
  def withFilter(f: T => Boolean) = filter(f)

  //endregion
}

object Traversable extends UnfoldFactory[Traversable] {

  // CONSTRUCTORS

  object empty extends Traversable[Nothing] {
    def foreach[U](f: Nothing => U): Unit = {}
  }

  def single[T](e: T): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U) = f(e)
  }

  def unfold[S, T](s0: S)(f: S => (S, T)): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](g: T => U) = {
      var s = s0
      var t = default[T]
      while (true) {
        val (newS, newT) = f(s)
        s = newS
        t = newT
        g(t)
      }
    }
  }

  // TYPECLASS INSTANCES

  implicit object Monad extends MonadCombine[Traversable] {
    def combineK[A](x: Traversable[A], y: Traversable[A]) = x ++ y
    def tailRecM[A, B](a: A)(f: (A) => Traversable[Either[A, B]]): Traversable[B] = ???
    def pure[A](x: A): Traversable[A] = Traversable.single(x)
    def flatMap[X, Y](mx: Traversable[X])(f: X => Traversable[Y]) = mx.flatMap(f)
    def empty[A] = Traversable.empty
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
    def unzipE = {
      val ak = ArraySeq.newBuilder[A]
      val av = ArraySeq.newBuilder[B]
      for ((k, v) <- underlying) {
        ak add k
        av add v
      }
      (ak.result, av.result)
    }

    /**
     * Builds a collection type with two type parameters using this traversable collection of pairs.
     * @example {{{
     *   ((1, 2), (3, 4), (5, 6)) to HashMap == {1 -> 2, 3 -> 4, 5 -> 6}
     * }}}
     */
    def to[M[_, _], EvA[_], EvB[_]](factory: Factory2[Tuple2, M, EvA, EvB])(implicit A: EvA[A], B: EvB[B]) = factory from underlying

  }

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

  class OptionallyCollected[T, U](self: Traversable[T], pf: T => Option[U]) extends AbstractTraversable[U] {
    def foreach[V](f: U => V) = {
      for (x <- self; u <- pf(x)) f(u)
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
        accum = f(accum, x)
        g(accum)
      }
    }
  }

  class Consecutive[T, U](self: Traversable[T], f: (T, T) => U) extends AbstractTraversable[U] {
    var first = true
    var prev: T = default[T]
    def foreach[V](g: U => V) = {
      for (x <- self) {
        if (first) {
          prev = x
          first = false
        }
        else {
          g(f(prev, x))
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
    override def sizeKnown = self.sizeKnown
    override def size = Math.min(n, self.size)
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
    override def sizeKnown = self.sizeKnown
    override def size = Math.max(0, self.size - n)
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

  class DroppedTo[T](self: Traversable[T], f: T => Boolean) extends AbstractTraversable[T] {
    def foreach[U](g: T => U) = {
      var goal = false
      for (x <- self) {
        if (goal) g(x)
        else if (f(x)) goal = true
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

  class Sliding[T](self: Traversable[T], n: Int, step: Int) extends AbstractTraversable[IndexedSeq[T]] {
    def foreach[V](f: IndexedSeq[T] => V) = {
      var buf = ArraySeq.withSizeHint[T](n)
      for (x <- self) {
        buf :+= x
        if (buf.size == n) {
          f(buf)
          buf = buf drop step to ArraySeq
        }
      }
    }
  }

  class Chunked[T](self: Traversable[T], n: Int) extends AbstractTraversable[IndexedSeq[T]] {
    def foreach[V](f: IndexedSeq[T] => V) = {
      var buf = ArraySeq.withSizeHint[T](n)
      for (x <- self) {
        buf :+= x
        if (buf.size == n) {
          f(buf)
          buf = ArraySeq.withSizeHint[T](n)
        }
      }
      if (buf.size > 0) f(buf)
    }
  }

  class Interspersed[T](self: Traversable[T], u: T) extends AbstractTraversable[T] {
    def foreach[V](f: T => V) = {
      var first = true
      for (x <- self) {
        f(x)
        if (first) first = false
        else f(u)
      }
    }
  }

  class ZippedWith[T, U, X](self: Traversable[T], that: Iterable[U], f: (T, U) => X) extends AbstractTraversable[X] {
    def foreach[V](g: X => V): Unit = {
      val iu = that.newIterator
      for (t <- self) {
        if (!iu.advance()) return
        g(f(t, iu.current))
      }
    }
  }


}

abstract class AbstractTraversable[+T] extends Traversable[T]

