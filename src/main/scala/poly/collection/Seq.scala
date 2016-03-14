package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.ops._
import poly.algebra.implicits._
import poly.algebra.function._
import poly.collection.mut._
import poly.collection.node._
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents sequences that guarantee the same order every time it is traversed,
 * henceforth we can talk about indices on sequences.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Seq[+T] extends Iterable[T] with IntKeyedSortedMap[T] { self =>

  import Seq._

  /** Returns a dummy node whose next node is the head of this sequence. */
  def dummy: SeqNode[T]

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
  override def apply(i: Int): T = {
    var node = headNode
    var j = 0
    while (j < i) {
      node = node.next
      j += 1
    }
    node.data
  }

  /** Returns the weak order on keys. In this case (`Seq`), it returns the total order on integers. */
  def orderOnKey = TotalOrder[Int]

  override def size = length

  override def foreach[V](f: T => V): Unit = {
    var node = headNode
    while (node.notDummy) {
      f(node.data)
      node = node.next
    }
  }

  def newIterator: Iterator[T] = new Iterator[T] {
    private[this] var node: SeqNode[T] = self.dummy
    def advance() = {
      node = node.next
      node.notDummy
    }
    def current = node.data
  }

  override def isDefinedAt(i: Int) = containsKey(i)

  def ?(i: Int) = if (i >= 0 && i < length) Some(this(i)) else None

  def containsKey(i: Int) = i >= 0 && i < length

  /**
    * Pairs each element with its index. $LAZY
    * @example {{{('a', 'b', 'c').pairs == ((0, 'a'), (1, 'b'), (2, 'c'))}}}
    * @return A sequence of index-element pairs.
    */
  def pairs: SortedSeq[(Int, T @uv)] = {
    class SeqNodeWithIndex(val outer: SeqNode[T], val i: Int) extends SeqNode[(Int, T)] {
      def data = (i, outer.data)
      def next = new SeqNodeWithIndex(outer.next, i + 1)
      def isDummy = outer.isDummy
    }
    ofHeadNode(new SeqNodeWithIndex(headNode, 0)).asIfSorted[(Int, T)](WeakOrder by firstOfPair)
  }

  override def keys = pairs map firstOfPair

  // HELPER FUNCTIONS

  override def isEmpty = headNode.isDummy

  override def map[U](f: T => U): Seq[U] = ofHeadNode(self.headNode map f)

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

  def product[U](that: Seq[U]): Seq[(T, U)] = this.flatMap(t => that.map(u => (t, u)))

  override def filter(f: T => Boolean): Seq[T] = {
    class FilteredSeqNode(val node: SeqNode[T]) extends SeqNode[T] {
      override def isDummy = node.isDummy
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

  def concat[U >: T](that: Seq[U]): Seq[U] = {
    class ConcatenatedSeqNode(val first: Boolean, val node: SeqNode[U]) extends SeqNode[U] {
      override def isDummy = !first && node.isDummy
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

  override def scanByMonoid[U >: T : Monoid] = scanLeft(id[U])(_ op _)

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

  def headNode = dummy.next

  override def tail: Seq[T] = ofHeadNode(headNode.next)

  /**
    * Returns the list of tails of this sequence. $LAZY
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

  override def skip(n: Int) = {
    var node = self.headNode
    var i = 0
    while (node.notDummy && i < n) {
      node = node.next
      i += 1
    }
    ofHeadNode(node)
  }

  override def skipWhile(f: T => Boolean) = {
    var node = self.headNode
    while (node.notDummy && f(node.data))
      node = node.next
    ofHeadNode(node)
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

  override def takeUntil(f: T => Boolean) = takeWhile(x => !f(x))

  override def slice(i: Int, j: Int) = self.skip(i).take(j - i)

  override def distinct[U >: T : IntHashing]: Seq[T] = {
    val set = HashSet[U]()
    class DistinctNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next: DistinctNode = {
        var n = outer.next
        while (n.notDummy) {
          if (set notContains n.data) {
            set add n.data
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

  override def distinctBy[U: IntHashing](f: T => U): Seq[T] = {
    val set = HashSet[U]()
    class DistinctByNode(outer: SeqNode[T]) extends SeqNode[T] {
      def next: DistinctByNode = {
        var n = outer.next
        while (n.notDummy) {
          val u = f(n.data)
          if (set notContains u) {
            set add u
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

  def union[U >: T : IntHashing](that: Seq[U]): Seq[U] = (this concat that).distinct

  def intersect[U >: T : IntHashing](that: Seq[U]): Seq[U] = (this filter that.to[HashSet]).distinct

  override def rotate(i: Int) = self.skip(i) ++ self.take(i)

  override def repeat(n: Int): Seq[T] = {
    if (n <= 0) return Seq.empty
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

  /**
   * Pretends that this sequence is sorted under the given implicit order.
   * @param U The implicit order
   * @note Actual orderedness is not guaranteed! The user should make sure that it is sorted.
   * @return A sorted sequence
   */
  override def asIfSorted[U >: T](implicit U: WeakOrder[U]): SortedSeq[T@uv] = new SortedSeq[T] {
    def order = U
    def dummy: SeqNode[T] = self.dummy
  }

  def zip[U](that: Seq[U]): Seq[(T, U)] = ofDummyNode(self.dummy zip that.dummy)


  // INDEXING OPERATIONS

  def firstIndexOf[U >: T](x: U): Int = {
    var i = 0
    for (y ← this) {
      if (y == x) return i
      i += 1
    }
    -1
  }

  def lastIndexOf[U >: T](x: U): Int = {
    var i = 0
    var k = -1
    for (y ← this) {
      if (y == x) k = i
      i += 1
    }
    k
  }

  def firstIndexWhere(f: T => Boolean): Int = {
    var i = 0
    for (y ← this) {
      if (f(y)) return i
      i += 1
    }
    -1
  }

  def lastIndexWhere(f: T => Boolean): Int = {
    var i = 0
    var k = -1
    for (y ← this) {
      if (f(y)) k = i
      i += 1
    }
    k
  }

  def startsWith[U >: T : Equiv](pattern: Seq[U]) = {
    ???
  }

  def endsWith[U >: T : Equiv](pattern: Seq[U]) = {
    ???
  }

  def asSeq: Seq[T] = ofDummyNode(dummy)

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Equiv[T](poly.algebra.Equiv.default[T]).eq(this, that)
    case _ => false
  }

  override def toString = "Seq(" + buildString(",") + ")" // overridden the `toString` in Map

  override def hashCode = ???

  override def |>[U](f: T => U): Seq[U] = this map f
  override def +:[U >: T](u: U): Seq[U] = this prepend u
  override def :+[U >: T](u: U): Seq[U] = this append u
  def ++[U >: T](that: Seq[U]) = this concat that
}

object Seq {

  // EXTRACTORS

  /** Decomposes a sequence into its head and its tail. */
  def unapply[T](xs: Seq[T]) = {
    if (xs.isEmpty) None
    else Some((xs.head, xs.tail))
  }

  // CONSTRUCTORS

  object empty extends Seq[Nothing] {
    def dummy = SeqNode.dummy
  }

  def ofDummyNode[T](d: SeqNode[T]): Seq[T] = new AbstractSeq[T] {
    def dummy = d
  }

  def ofHeadNode[T](h: SeqNode[T]): Seq[T] = new AbstractSeq[T] {
    def dummy = new SeqNode[T] {
      def data = throw new NoSuchElementException
      def next = h
      def isDummy = true
    }
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
    ofDummyNode(new InfiniteSeqNode)
  }

  implicit def Equiv[T: Equiv]: Equiv[Seq[T]] = new Equiv[Seq[T]] {
    def eq(x: Seq[T], y: Seq[T]): Boolean = { //TODO: faster implementation using iterators?
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
  implicit def LexicographicOrder[T: WeakOrder]: WeakOrder[Seq[T]] = new WeakOrder[Seq[T]] {
    def cmp(x: Seq[T], y: Seq[T]): Int = { //TODO: faster implementation using iterators?
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
    def id[X](u: X) = ListSeq(u)
    def flatMap[X, Y](mx: Seq[X])(f: X => Seq[Y]) = mx flatMap f
    def empty[X] = Seq.empty
    def concat[X](sx: Seq[X], sy: Seq[X]) = sx concat sy
  }

  implicit object Comonad extends Comonad[Seq] {
    def id[X](u: Seq[X]) = u.head
    def extend[X, Y](wx: Seq[X])(f: Seq[X] => Y) = wx.suffixes.map(f)
  }
}

abstract class AbstractSeq[+T] extends AbstractIterable[T] with Seq[T]
