package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._
import poly.collection.node._
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents sequences that guarantee the same order every time it is traversed,
 * henceforth indices on sequences are well-defined.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Seq[+T] extends Iterable[T] with IntKeyedSortedMap[T] { self =>

  import Seq._

  /** Returns the head node of this sequence. */
  def headNode: SeqNode[T]

  /** Returns a dummy node whose next node is the head of this sequence. */
  def dummy: SeqNode[T] = new SeqT.DummyNode[T](self)

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
    if (i < 0) {
      val l = length
      if (i < -l) throw new KeyNotFoundException(i)
      else apply(i + l)
    }
    else {
      var node = headNode
      var j = 0
      while (j < i) {
        node = node.next
        j += 1
      }
      node.data
    }
  }

  override def size = length

  override def foreach[V](f: T => V): Unit = {
    var node = headNode
    while (node.notDummy) {
      f(node.data)
      node = node.next
    }
  }

  def newIterator: Iterator[T] = new SeqT.DefaultIterator(self)

  def orderOnKeys = Order[Int]

  def keys: SortedSeq[Int] = new SeqT.Keys(self)

  def ?(i: Int) = if (i >= 0 && i < length) Some(this(i)) else None

  def containsKey(i: Int) = i >= 0 && i < length

  override def pairs: SortedSeq[(Int, T @uv)] = new SeqT.Pairs(self)

  // HELPER FUNCTIONS

  override def isEmpty = headNode.isDummy

  override def map[U](f: T => U): Seq[U] = new SeqT.Mapped(self, f)

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
          var newInner = f(newOuter.data).headNode
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

  def monadicProduct[U](that: Seq[U]): Seq[(T, U)] = for (t <- this; u <- that) yield (t, u)

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

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

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

  override def scanLeft[U](z: U)(f: (U, T) => U): Seq[U] = {
    class ScannedNode(val outer: SeqNode[T], val data: U) extends SeqNode[U] {
      def next = {
        if (outer.next.notDummy) new ScannedNode(outer.next, f(data, outer.next.data))
        else SeqNode.dummy
      }
      def isDummy = false
    }
    ofHeadNode(new ScannedNode(self.dummy, z))
  }

  override def scan[U >: T](z: U)(f: (U, U) => U) = scanLeft(z)(f)

  override def scanByMonoid[U >: T : Monoid] = scanLeft(id[U])(_ <> _)

  override def consecutive[U](f: (T, T) => U): Seq[U] = {
    class DiffNode(val a: SeqNode[T], val b: SeqNode[T]) extends SeqNode[U] {
      def data = f(b.data, a.data)
      def next = new DiffNode(b, b.next)
      def isDummy = a.isDummy || b.isDummy
    }
    ofDummyNode(new DiffNode(self.dummy, self.headNode))
  }

  override def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) => U.op(y, U.inv(x)))

  override def head = headNode.data

  override def tail: Seq[T] = ofHeadNode(headNode.next)

  /**
    * Returns the list of tails of this sequence. $LAZY
   *
   * @example {{{(1, 2, 3).suffixes == ((1, 2, 3), (2, 3), (3))}}}
    */
  override def suffixes = {
    class TailsNode(val outer: SeqNode[T]) extends SeqNode[Seq[T]] {
      def data = ofHeadNode(outer)
      def next = new TailsNode(outer.next)
      def isDummy = outer.isDummy
    }
    ofDummyNode(new TailsNode(self.dummy))
  }

  override def init = {
    class InitNode(val n0: SeqNode[T], val n1: SeqNode[T]) extends SeqNode[T] {
      def data = n0.data
      def next = new InitNode(n1, n1.next)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofDummyNode(new InitNode(self.dummy, self.headNode))
  }

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

  override def distinct[U >: T : Eq]: Seq[T] = {
    val set = AutoSet[U]()
    class DistinctNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next: DistinctNode = {
        var n = outer.next
        while (n.notDummy) {
          if (set notContains n.data) {
            set += n.data
            return new DistinctNode(n)
          }
          n = n.next
        }
        new DistinctNode(n)
      }
      def data = outer.data
      def isDummy = outer.isDummy
    }
    ofDummyNode(new DistinctNode(self.dummy))
  }

  override def distinctBy[U: Eq](f: T => U): Seq[T] = {
    val set = AutoSet[U]()
    class DistinctByNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next: DistinctByNode = {
        var n = outer.next
        while (n.notDummy) {
          val u = f(n.data)
          if (set notContains u) {
            set addInplace u
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

  def intersect[U >: T : Eq](that: Seq[U]): Seq[U] = (this filter AutoSet.from(that)).distinct

  override def rotate(i: Int) = self.drop(i) ++ self.take(i)

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

  override def reverse: BiSeq[T] = self.to(ArraySeq).reverse


  override def asIfSorted(implicit T: Order[T]): SortedSeq[T @uv] = new SortedSeq[T] {
    def orderOnElements = T
    def headNode: SeqNode[T] = self.headNode
  }

  def zip[U](that: Seq[U]): Seq[(T, U)] = (self zipWith that) { (t, u) => (t, u) }

  def zipWith[U, V](that: Seq[U])(f: (T, U) => V): Seq[V] = {
    class ZippedWithNode(nt: SeqNode[T], nu: SeqNode[U]) extends SeqNode[V] {
      def next = new ZippedWithNode(nt.next, nu.next)
      def data = f(nt.data, nu.data)
      def isDummy = nt.isDummy || nu.isDummy
    }
    ofHeadNode(new ZippedWithNode(self.headNode, that.headNode))
  }

  // INDEXING OPERATIONS

  /** Finds the index of the first occurrence of a given value in this sequence. */
  def firstIndexOf[U >: T : Eq](x: U) = firstIndexWhere(x === _)

  /** Finds the index of the last occurrence of a given value in this sequence. */
  def lastIndexOf[U >: T : Eq](x: U) = lastIndexWhere(x === _)

  def firstIndexWhere(f: T => Boolean): Int = {
    var i = 0
    for (y <- self) {
      if (f(y)) return i
      i += 1
    }
    -1
  }

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
   * @example {{{
   *   (1, 2, 3, 4) startsWith (1, 2) == true
   * }}}
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
      if (j.current !== i.current)
        return false
    }
    false
  }

  /**
   * Tests if this sequence ends with the pattern sequence.
   * @example {{{
   *   (1, 2, 3, 4) endsWith (3, 4) == true
   * }}}
   */
  def endsWith[U >: T : Eq](pattern: Seq[U]) = self.reverse startsWith pattern.reverse

  def asSeq: Seq[T] = ofHeadNode(headNode)

  // SYMBOLIC ALIASES

  override def |>[U](f: T => U) = this map f

  override def +:[U >: T](u: U): Seq[U] = this prepend u
  override def :+[U >: T](u: U): Seq[U] = this append u
  def ++[U >: T](that: Seq[U]) = this concat that
 // override def *(n: Int) = this repeat n
  def ⋈[U](that: Seq[U]) = this zip that

  override def withFilter(f: T => Boolean) = filter(f)

  // OVERRIDING JAVA DEFAULT METHODS

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Eq[T](poly.algebra.Eq.default[T]).eq(this, that)
    case _ => false
  }

  override def toString = super[Iterable].toString

  override def hashCode = MurmurHash3.sequentialHash(self)(Hashing.default[T])

}

object Seq extends FactoryA[Seq] {

  // EXTRACTORS

  /** Decomposes a sequence into its head and its tail. */
  def unapply[T](xs: Seq[T]) = {
    if (xs.isEmpty) None
    else Some((xs.head, xs.tail))
  }

  // CONSTRUCTORS

  override def apply[T](xs: T*) =
    arrayAsPoly(getArrayFromVarargs(xs)) // directly wraps the inner Scala array without element copying

  def from[T](xs: Traversable[T]) = xs to ArraySeq

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

  def iterate[T](s: T)(f: T => T): Seq[T] = {
    class IteratedSeqNode(val data: T) extends SeqNode[T] {
      def next = new IteratedSeqNode(f(data))
      def isDummy = false
    }
    ofHeadNode(new IteratedSeqNode(s))
  }

  def infinite[T](x: => T): Seq[T] = {
    class InfiniteSeqNode extends SeqNode[T] {
      def next = this
      def data = x
      def isDummy = false
    }
    ofHeadNode(new InfiniteSeqNode)
  }

  // TYPECLASS INSTANCES

  //TODO: should be implicit, but results in ambiguous implicits because of problems with contravariant typeclass (SI-2509)
  def Eq[T: Eq]: Eq[Seq[T]] = new Eq[Seq[T]] {
    def eq(x: Seq[T], y: Seq[T]): Boolean = {
      //TODO: faster implementation using iterators?
      var xn = x.headNode
      var yn = y.headNode
      while (xn.notDummy && yn.notDummy) {
        if (xn.data !== yn.data) return false
        xn = xn.next
        yn = yn.next
      }
      if (xn.notDummy) return false
      if (yn.notDummy) return false
      true
    }
  }

  /**
   * Returns the lexicographic order on sequences if an order on the elements is given.
   */
  def LexicographicOrder[T: Order]: Order[Seq[T]] = new Order[Seq[T]] {
    def cmp(x: Seq[T], y: Seq[T]): Int = {
      //TODO: faster implementation using iterators?
      var xn = x.headNode
      var yn = y.headNode
      while (xn.notDummy && yn.notDummy) {
        val cmp = xn.data >?< yn.data
        if (cmp != 0) return cmp
        xn = xn.next
        yn = yn.next
      }
      if (xn.isDummy && yn.isDummy) 0
      else if (xn.isDummy) -1 else 1
    }
  }

  implicit object Monad extends ConcatenativeMonad[Seq] {
    def id[X](u: X) = mut.ListSeq(u)
    def flatMap[X, Y](mx: Seq[X])(f: X => Seq[Y]) = mx flatMap f
    def empty[X] = Seq.Empty
    def concat[X](sx: Seq[X], sy: Seq[X]) = sx concat sy
  }

  implicit def FreeMonoid[T]: ConcatenativeMonoid[Seq[T]] = new ConcatenativeMonoid[Seq[T]] {
    def concat(x: Seq[T], y: Seq[T]) = x ++ y
    def empty = Seq.Empty
  }

  implicit object Comonad extends Comonad[Seq] {
    //TODO: actually should be Comonad[NonEmptySeq] ?
    def id[X](u: Seq[X]) = u.head
    def extend[X, Y](wx: Seq[X])(f: Seq[X] => Y) = wx.suffixes.map(f)
  }
}

abstract class AbstractSeq[+T] extends AbstractIterable[T] with Seq[T]

private[poly] object SeqT {

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
    def orderOnElements = Order[Int] contramap first
    def headNode = new SeqNodeWithIndex(self.headNode, 0)
  }

  class Keys[T](self: Seq[T]) extends AbstractSortedSeq[Int] {
    class IndexNode(val outer: SeqNode[T], val i: Int) extends SeqNode[Int] {
      def next = new IndexNode(outer.next, i + 1)
      def data = i
      def isDummy = outer.isDummy
    }
    def headNode = new IndexNode(self.headNode, 0)
    def orderOnElements = Order[Int]
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