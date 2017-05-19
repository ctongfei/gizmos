package poly.collection

import cats.implicits._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._
import poly.collection.node._
import poly.collection.typeclass._

import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents sequences that guarantee the same order every time it is traversed,
 * henceforth indices on sequences are well-defined.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Seq[+T] extends Iterable[T] with PartialFunction[Int, T] { self =>

  import Seq._

  /** Returns the head node of this sequence. */
  def headNode: SeqNode[T]

  /** Returns a dummy node whose next node is the head of this sequence. */
  def dummy: SeqNode[T] = new DummyNode[T](self)

  /** Returns the length of this sequence. $On */
  def length: Int = {
    var node = headNode
    var n = 0
    while (node.notDummy) {
      node = node.next
      n += 1
    }
    n
  }

  /**
   * Returns the ''i''-th element of this sequence. $On
   * @param i Index
   * @return The ''i''-th element of this sequence
   */
  def apply(i: Int): T = {
    var node = headNode
    var j = 0
    while (j < i) {
      node = node.next
      j += 1
    }
    node.data
  }

  def isDefinedAt(i: Int) = 0 <= i && i < length //TODO: faster implementation; skip calculation of length

  override def size = length

  override def foreach[V](f: T => V): Unit = {
    var node = headNode
    while (node.notDummy) {
      f(node.data)
      node = node.next
    }
  }

  def newIterator: Iterator[T] = new DefaultIterator(self)

  override def isEmpty = headNode.isDummy

  def indexSet = Range(length).asSet

  /** Returns the indices of this sequence. */
  def indices: SortedSeq[Int] = new Keys(self)

  //region MONADIC OPS

  override def map[U](f: T => U): Seq[U] = new Mapped(self, f)

  def flatMap[U](f: T => Seq[U]): Seq[U] = {
    class FlatMappedSeqNode(val outer: SeqNode[T], val inner: SeqNode[U]) extends SeqNode[U] {
      override def isDummy = outer.isDummy
      def data = inner.data
      def next: FlatMappedSeqNode = {
        val innerNext = inner.next
        if (innerNext.notDummy)
          new FlatMappedSeqNode(outer, innerNext) // advances the inner node
        else {
          var newOuter = outer.next
          var newInner = if (newOuter.isDummy) SeqNode.dummy else f(newOuter.data).headNode
          while (newOuter.notDummy) {
            if (newInner.notDummy) return new FlatMappedSeqNode(newOuter, newInner)
            newOuter = newOuter.next
            newInner = f(newOuter.data).headNode
          }
          new FlatMappedSeqNode(newOuter, newInner) // iteration complete, no more elements
        }
      }
    }
    ofDummyNode(new FlatMappedSeqNode(dummy, SeqNode.dummy))
  }

  def product[U](that: Seq[U]): Seq[(T, U)] = for (t <- this; u <- that) yield (t, u)

  def productWith[U, X](that: Seq[U])(f: (T, U) => X): Seq[X] = for (t <- this; u <- that) yield f(t, u)
  //endregion

  //region IDIOMATIC OPS

  def zip[U](that: Seq[U]): Seq[(T, U)] = (self zipWith that) { (t, u) => (t, u) }

  def zipWith[U, V](that: Seq[U])(f: (T, U) => V): Seq[V] = {
    class ZippedWithNode(nt: SeqNode[T], nu: SeqNode[U]) extends SeqNode[V] {
      def next = new ZippedWithNode(nt.next, nu.next)
      def data = f(nt.data, nu.data)
      def isDummy = nt.isDummy || nu.isDummy
    }
    ofHeadNode(new ZippedWithNode(self.headNode, that.headNode))
  }
  //endregion

  //region FILTERING OPS

  override def filter(f: T => Boolean): Seq[T] = {
    class FilteredSeqNode(val node: SeqNode[T]) extends SeqNode[T] {
      def isDummy = node.isDummy
      def data = node.data
      def next = {
        var nextNode = node.next
        while (nextNode.notDummy && !f(nextNode.data)) nextNode = nextNode.next
        new FilteredSeqNode(nextNode)
      }
    }
    ofDummyNode(new FilteredSeqNode(dummy))
  }

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

  override def collect[U](pf: PartialFunction[T, U]): Seq[U] = {
    class CollectedNode(val node: SeqNode[T], val data: U) extends SeqNode[U] {
      def next = {
        var nextNode = node.next
        var u = default[U]
        while (nextNode.notDummy && !(pf runWith { u = _ })(nextNode.data)) nextNode = nextNode.next
        new CollectedNode(nextNode, u)
      }
      def isDummy = node.isDummy
    }
    ofDummyNode(new CollectedNode(dummy, default[U]))
  }

  override def collectOption[U](f: T => Option[U]): Seq[U] = collect(Function.unlift(f))

  //endregion

  //region SET OPS

  override def distinct[U >: T](implicit U: Eq[U]): Seq[U] = distinctBy[U](identity)

  override def distinctBy[U: Eq](f: T => U): Seq[T] = {
    val set = AutoSet[U]()
    class DistinctByNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next: DistinctByNode = {
        var n = outer.next
        while (n.notDummy) {
          val u = f(n.data)
          if (set notContains u) {
            set add_! u
            return new DistinctByNode(n)
          }
          n = n.next
        }
        new DistinctByNode(n)
      }
      def data = outer.data
      def isDummy = outer.isDummy
    }
    ofDummyNode(new DistinctByNode(self.dummy))
  }

  def union[U >: T : Eq](that: Seq[U]): Seq[U] = (this concat that).distinct

  def intersect[U >: T : Eq](that: Seq[U]): Seq[U] = (this filter AutoSet.from(that)).distinct[U]

  //endregion

  //region SEQUENCE OPS

  def concat[U >: T](that: Seq[U]): Seq[U] = {
    class ConcatenatedSeqNode(val first: Boolean, val node: SeqNode[U]) extends SeqNode[U] {
      def isDummy = !first && node.isDummy
      def data = node.data
      def next = {
        if (!first) new ConcatenatedSeqNode(false, node.next)
        else if (node.next.isDummy) new ConcatenatedSeqNode(false, that.headNode)
        else new ConcatenatedSeqNode(true, node.next)
      }
    }
    ofHeadNode(new ConcatenatedSeqNode(true, self.headNode))
  }

  override def prepend[U >: T](x: U): Seq[U] = {
    val prependedNode: SeqNode[U] = new SeqNode[U] {
      def isDummy = false
      def data = x
      def next = self.headNode
    }
    ofHeadNode(prependedNode)
  }

  override def append[U >: T](x: U): Seq[U] = {
    class AppendedSeqNode(val outer: SeqNode[U], val lastPassed: Boolean) extends SeqNode[U] {
      def isDummy = outer.isDummy && lastPassed
      def data = if (outer.notDummy) outer.data else x
      def next = if (outer.isDummy) new AppendedSeqNode(outer, true) else new AppendedSeqNode(outer.next, false)
    }
    ofHeadNode(new AppendedSeqNode(self.headNode, false))
  }

  override def head = headNode.data

  override def tail: Seq[T] = ofHeadNode(headNode.next)

  override def init = {
    class InitNode(val n0: SeqNode[T], val n1: SeqNode[T]) extends SeqNode[T] {
      def data = n0.data
      def next = new InitNode(n1, n1.next)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofDummyNode(new InitNode(self.dummy, self.headNode))
  }

  override def suffixes = { // comonadic operation!
    class TailsNode(val outer: SeqNode[T]) extends SeqNode[Seq[T]] {
      def data = ofHeadNode(outer)
      def next = new TailsNode(outer.next)
      def isDummy = outer.isDummy
    }
    ofDummyNode(new TailsNode(self.dummy))
  }

  override def take(n: Int) = {
    class TakenNode(val i: Int, val outer: SeqNode[T]) extends SeqNode[T] {
      def data = outer.data
      def next = new TakenNode(i + 1, outer.next)
      def isDummy = outer.isDummy || i >= n
    }
    ofDummyNode(new TakenNode(-1, self.dummy))
  }

  override def takeWhile(f: T => Boolean) = {
    class TakenWhileNode(val outer: SeqNode[T]) extends SeqNode[T] {
      def isDummy = outer.isDummy || !f(outer.data)
      def data = outer.data
      def next = if (!f(outer.next.data)) SeqNode.dummy else new TakenWhileNode(outer.next)
    }
    ofDummyNode(new TakenWhileNode(self.dummy))
  }

  override def takeTo(f: T => Boolean) = {
    class TakenToNode(val outer: SeqNode[T]) extends SeqNode[T] {
      def data = outer.data
      def next = if (outer.notDummy && f(outer.data)) SeqNode.dummy else new TakenToNode(outer.next)
      def isDummy = outer.isDummy
    }
    ofDummyNode(new TakenToNode(self.dummy))
  }

  override def takeUntil(f: T => Boolean) = takeWhile(!f)

  override def drop(n: Int) = ofHeadNode {
    var node = self.headNode
    var i = 0
    while (node.notDummy && i < n) {
      node = node.next
      i += 1
    }
    node
  }

  override def dropWhile(f: T => Boolean) = ofHeadNode {
    var node = self.headNode
    while (node.notDummy && f(node.data))
      node = node.next
    node
  }

  override def dropTo(f: T => Boolean) = ofHeadNode {
    var node = self.headNode
    while (node.notDummy && !f(node.data))
      node = node.next
    node.next
  }

  override def dropUntil(f: T => Boolean) = dropWhile(!f)

  override def slice(i: Int, j: Int) = {
    if (i >= 0 && j >= 0)
      self.drop(i).take(j - i)
    else {
      val l = length
      val ii = if (i < 0) i + l else i
      val jj = if (j < 0) j + l else j
      self.drop(ii).take(jj - ii)
    }
  }

  override def withIndex: SortedSeq[(Int, T @uv)] = new Pairs(self)

  override def repeat(n: Int): Seq[T] = {
    if (n <= 0) return Seq.Empty
    class RepeatedNode(i: Int, outer: SeqNode[T]) extends SeqNode[T] {
      def next = {
        val tempNext = outer.next
        if (tempNext.isDummy) {
          if (i >= n - 1) SeqNode.dummy
          else new RepeatedNode(i + 1, self.headNode)
        }
        else new RepeatedNode(i, tempNext)
      }
      def data = outer.data
      def isDummy = false
    }
    ofHeadNode(new RepeatedNode(0, self.headNode))
  }

  override def cycle = {
    class CycleNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next = {
        val tempNext = outer.next
        if (tempNext.isDummy) new CycleNode(self.headNode) else new CycleNode(tempNext)
      }
      def data = outer.data
      def isDummy = false
    }
    ofHeadNode(new CycleNode(self.headNode))
  }

  //endregion

  //region FOLDING/SCANNING OPS

  override def scanLeft[U](z: U)(f: (U, T) => U): Seq[U] = {
    class ScannedNode(val outer: SeqNode[T], val data: U) extends SeqNode[U] {
      def next = {
        if (outer.next.notDummy) new ScannedNode(outer.next, f(data, outer.next.data))
        else SeqNode.dummy
      }
      def isDummy = false
    }
    ofDummyNode(new ScannedNode(self.dummy, z))
  }

  override def scanRight[U](z: U)(f: (T, U) => U): BidiSeq[U] =
    self.reverse.scanLeft(z)((x, y) => f(y, x)).reverse

  override def scanLeftByMonoid[U >: T](implicit U: Monoid[U]) = scanLeft(U.empty)(U.combine)

  override def unscanLeft[U >: T, V](z: U)(f: (U, U) => V) = (z +: self) slidingPairsWith f

  override def unscanRight[U >: T, V](z: U)(f: (U, U) => V) = (self :+ z) slidingPairsWith f

  override def unscanLeftByGroup[U >: T](implicit U: Group[U]) = unscanLeft(U.empty)((x, y) => U.remove(y, x))

  override def unscanRightByGroup[U >: T](implicit U: Group[U]) = unscanRight(U.empty)(U.remove)

  //endregion

  //region SEQUENTIAL GROUPING OPS

  override def slidingPairsWith[U](f: (T, T) => U): Seq[U] = {
    class DiffNode(val a: SeqNode[T], val b: SeqNode[T]) extends SeqNode[U] {
      def data = f(a.data, b.data)
      def next = new DiffNode(b, b.next)
      def isDummy = a.isDummy || b.isDummy
    }
    ofDummyNode(new DiffNode(self.dummy, self.headNode))
  }

  override def slidingPairs = slidingPairsWith { (x, y) => (x, y) }

  //endregion

  //region REORDERING OPS

  override def rotate(i: Int) = {
    val m = i %+ length
    self.drop(m) ++ self.take(m)
  }

  override def reverse: BidiSeq[T] = self.to(ArraySeq).reverse

  //endregion

  //region INDEX OPS

  /** Finds the index of the first occurrence of a given value in this sequence. */
  def firstIndexOf[U >: T : Eq](x: U) = firstIndexWhere(x === _)

  /** Finds the index of the last occurrence of a given value in this sequence. */
  def lastIndexOf[U >: T : Eq](x: U) = lastIndexWhere(x === _)

  /** Finds the index of the first occurrence of an element that satisfies the given predicate in this sequence. */
  def firstIndexWhere(f: T => Boolean): Int = {
    var i = 0
    for (y <- self) {
      if (f(y)) return i
      i += 1
    }
    -1
  }

  /** Finds the index of the last occurrence of an element that satisfies the given predicate in this sequence. */
  def lastIndexWhere(f: T => Boolean): Int = {
    var i = 0
    var k = -1
    for (y <- self) {
      if (f(y)) k = i
      i += 1
    }
    k
  }

  /**
   * Tests if this sequence starts with the pattern sequence.
   * @example {{{ (1, 2, 3, 4) startsWith (1, 2) == true }}}
   */
  def startsWith[U >: T : Eq](pattern: Seq[U]): Boolean = {
    val i = self.newIterator
    val j = pattern.newIterator
    var iHasNext = false
    var jHasNext = false
    while (true) {
      iHasNext = i.advance()
      jHasNext = j.advance()
      if (!iHasNext || !jHasNext) return !jHasNext
      if (j.current =!= i.current)
        return false
    }
    false
  }

  /**
   * Tests if this sequence ends with the pattern sequence.
   * @example {{{ (1, 2, 3, 4) endsWith (3, 4) == true }}}
   */
  def endsWith[U >: T : Eq](pattern: Seq[U]) = self.reverse startsWith pattern.reverse
  //endregion

  //region DECORATION OPS

  override def asIfSorted[U >: T](implicit U: Order[U]): SortedSeq[U] = new SortedSeq[U] {
    def elementOrder = U
    def headNode: SeqNode[T] = self.headNode
  }

  def asIfNonEmpty: NonEmptySeq[T] = new NonEmptySeq[T] {
    def headNode = self.headNode
  }
  //endregion

  //region CASTING OPS

  def asSeq: Seq[T] = ofHeadNode(headNode)

  /**
   * Casts this sequence as a map which maps indices to values. $LAZY
   * @example {{{ (a, b, c).asMap == {0: a, 1: b, 2: c} }}}
   */
  def asMap: KeySortedMap[Int, T] = new AsMap(self)

  /**
   * Returns a factory object that can used to build sequences that uses the same structure as this sequence.
   */
  def factory: SeqFactory[Seq] = ArraySeq

  //endregion

  //region SYMBOLIC ALIASES

  override def +:[U >: T](u: U): Seq[U] = this prepend u
  override def :+[U >: T](u: U): Seq[U] = this append u
  def ++[U >: T](that: Seq[U]) = this concat that
  def :*(n: Int) = this repeat n
  def ⋈[U](that: Seq[U]) = this zip that

  //endregion

  //region JAVA/SCALA CONFORMATION

  override def withFilter(f: T => Boolean) = filter(f)

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Seq.Eq[T](Hash.default[T]).eqv(this, that)
    case _ => false
  }

  override def toString = super[Iterable].toString

  override def hashCode = MurmurHash3.sequentialHash(self)(Hash.default[T])
  //endregion

}

object Seq extends UnfoldFactory[Seq] {

  // EXTRACTORS

  /** Decomposes a sequence into its head and its tail. */
  def unapply[T](xs: Seq[T]) = {
    if (xs.isEmpty) None
    else Some((xs.head, xs.tail))
  }

  // CONSTRUCTORS

  def apply[T](xs: T*) =
    arrayAsPoly(getArrayFromVarargs(xs)) // directly wraps the inner Scala array without element copying

  object Empty extends Seq[Nothing] {
    def headNode = SeqNode.dummy
    def unapply[T](xs: Seq[T]) = xs.isEmpty
  }

  def ofDummyNode[T](d: SeqNode[T]): Seq[T] = new AbstractSeq[T] {
    override def dummy = d
    def headNode = d.next
  }

  def ofHeadNode[T](h: SeqNode[T]): Seq[T] = new AbstractSeq[T] {
    def headNode = h
  }

  def unfold[S, T](s0: S)(f: S => (S, T)): Seq[T] = {
    class UnfoldedNode(s: S, t: T) extends SeqNode[T] {
      def next = {
        val (newS, newT) = f(s)
        new UnfoldedNode(newS, newT)
      }
      def data: T = t
      def isDummy = false
    }
    val (s1, t1) = f(s0)
    ofHeadNode(new UnfoldedNode(s1, t1))
  }

  // TYPECLASS INSTANCES
  //TODO: should be implicit, but results in ambiguous implicits because of problems with contravariant typeclass (SI-2509)
  implicit def Eq[T: Eq]: Eq[Seq[T]] = new Eq[Seq[T]] {
    def eqv(x: Seq[T], y: Seq[T]): Boolean = {
      //TODO: faster implementation using iterators?
      var xn = x.headNode
      var yn = y.headNode
      while (xn.notDummy && yn.notDummy) {
        if (xn.data =!= yn.data) return false
        xn = xn.next
        yn = yn.next
      }
      if (xn.notDummy) return false
      if (yn.notDummy) return false
      true
    }
  }

  /** The cloner for sequences. */
  implicit def Cloning[T](implicit T: Cloning[T]): Cloning[Seq[T]] = new Cloning[Seq[T]] {
    def clone(x: Seq[T]): Seq[T] =
      x.factory.newSeqBuilder[T] <<<! x
  }

  /**
   * Returns the lexicographic order on sequences if an order on the elements is given.
   */
  def LexicographicOrder[T](implicit T: Order[T]): Order[Seq[T]] = new Order[Seq[T]] {
    def compare(x: Seq[T], y: Seq[T]): Int = {
      //TODO: faster implementation using iterators?
      var xn = x.headNode
      var yn = y.headNode
      while (xn.notDummy && yn.notDummy) {
        val cmp = T.compare(xn.data, yn.data)
        if (cmp != 0) return cmp
        xn = xn.next
        yn = yn.next
      }
      if (xn.isDummy && yn.isDummy) 0
      else if (xn.isDummy) -1 else 1
    }
  }

  /**
   * Returns the Levenshtein distance on sequences if an equivalence relation on the elements is given.
   */
  def LevenshteinDistance[T: Eq]: MetricSpace[Seq[T], Int] = new MetricSpace[Seq[T], Int] {
    import math._
    def distance(x: Seq[T], y: Seq[T]) = { // by dynamic programming
      val d = Array.ofDim[Int](x.length + 1, y.length + 1)
      for (i <- 0 to x.length) d(i)(0) = i
      for (j <- 0 to y.length) d(0)(j) = j
      for (j <- 1 to y.length; i <- 1 to x.length) {
        if (x(i - 1) == y(j - 1)) d(i)(j) = d(i - 1)(j - 1)
        else d(i)(j) = min(d(i - 1)(j), min(d(i)(j - 1), d(i - 1)(j - 1))) + 1
      }
      d(x.length)(y.length)
    }
  }

  implicit object Monad extends Monad[Seq] {
    def pure[X](u: X) = mut.ListSeq(u)
    def flatMap[X, Y](mx: Seq[X])(f: X => Seq[Y]) = mx flatMap f
    def tailRecM[A, B](a: A)(f: (A) => Seq[Either[A, B]]): Seq[B] = ??? //TODO: does not know how to implement this
  }

  //TODO: actually should be Comonad[NonEmptySeq] ?
  implicit object Comonad extends Comonad[Seq] {
    def extract[X](u: Seq[X]) = u.head
    def coflatMap[A, B](fa: Seq[A])(f: (Seq[A]) => B): Seq[B] = fa.suffixes map f
    def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa map f
  }



  class AsMap[T](self: Seq[T]) extends IntKeyedSortedMap[T] {
    def keySet = self.indexSet
    def ?(i: Int) = if (i >= 0 && i < self.length) Some(this(i)) else None
    def apply(k: Int) = self(k)
  }

  class DummyNode[T](self: Seq[T]) extends SeqNode[T] {
    def next = self.headNode
    def data = throw new DummyNodeException
    def isDummy = true
  }

  class DefaultIterator[T](self: Seq[T]) extends AbstractIterator[T] {
    private[this] var node: SeqNode[T] = self.dummy
    def advance() = {
      node = node.next
      node.notDummy
    }
    def current = node.data
  }

  class Pairs[T](self: Seq[T]) extends AbstractSortedSeq[(Int, T)] {
    class SeqNodeWithIndex(val outer: SeqNode[T], val i: Int) extends SeqNode[(Int, T)] {
      def data = (i, outer.data)
      def next = new SeqNodeWithIndex(outer.next, i + 1)
      def isDummy = outer.isDummy
    }
    def elementOrder = Order[Int] contramap first
    def headNode = new SeqNodeWithIndex(self.headNode, 0)
  }

  class Keys[T](self: Seq[T]) extends AbstractSortedSeq[Int] {
    class IndexNode(val outer: SeqNode[T], val i: Int) extends SeqNode[Int] {
      def next = new IndexNode(outer.next, i + 1)
      def data = i
      def isDummy = outer.isDummy
    }
    def headNode = new IndexNode(self.headNode, 0)
    def elementOrder = Order[Int]
  }

  class Mapped[T, U](self: Seq[T], f: T => U) extends AbstractSeq[U] {
    class MappedNode(val outer: SeqNode[T]) extends SeqNode[U] {
      def next = new MappedNode(outer.next)
      def data = f(outer.data)
      def isDummy = outer.isDummy
    }
    def headNode = new MappedNode(self.headNode)
    override def sizeKnown = self.sizeKnown // map preserves size
    override def size = self.size
  }

}

abstract class AbstractSeq[+T] extends AbstractIterable[T] with Seq[T]
