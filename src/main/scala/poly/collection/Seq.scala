package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.implicits._
import poly.algebra.function._
import poly.collection.node._
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents sequences that guarantee the same order every time it is traversed,
 * henceforth we can talk about indices on sequences.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Seq[+T] extends SortedMap[Int, T] with Iterable[T] { self =>

  import Seq._

  /** Returns the node that points to the head of this sequence. */
  def headNode: SeqNode[T]

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
    private[this] var node: SeqNode[T] = SeqNode.dummy
    private[this] var first = true
    def advance() = {
      if (first) {
        first = false
        node = headNode
      }
      else node = node.next
      node.notDummy
    }
    def current = node.data
  }

  override def isDefinedAt(i: Int) = containsKey(i)

  def ?(i: Int) = if (containsKey(i)) Some(this(i)) else None

  def containsKey(i: Int) = i >= 0 && i < size

  /**
    * Pairs each element with its index. $LAZY
    * @example {{{('a', 'b', 'c').pairs == ((0, 'a'), (1, 'b'), (2, 'c'))}}}
    * @return A sequence of index-element pairs.
    */
  def pairs: SortedSeq[(Int, T @uv)] = {
    var i = -1
    self.map(x => { i += 1; (i, x) }).asIfSorted(WeakOrder by first)
  }


  // HELPER FUNCTIONS

  override def isEmpty = headNode.isDummy

  override def map[U](f: T => U): Seq[U] = ofNode(self.headNode.map(f))

  def flatMap[U](f: T => Seq[U]): Seq[U] = {
    class FlatMappedSeqNode(val outer: SeqNode[T], val inner: SeqNode[U]) extends SeqNode[U] {
      override def isDummy = outer.isDummy
      def data = inner.data
      def next: FlatMappedSeqNode = {
        val innerNext = inner.next
        if (innerNext.notDummy)
          new FlatMappedSeqNode(outer, innerNext) // advances the inner node
        else {
          var newOuter = outer
          var newInner = f(newOuter.data).headNode
          while (newOuter.notDummy) {
            newOuter = newOuter.next
            newInner = f(newOuter.data).headNode
            if (newInner.notDummy) return new FlatMappedSeqNode(newOuter, newInner)
          }
          new FlatMappedSeqNode(newOuter, newInner) // iteration complete, no more elements
        }
      }
    }
    var outer = self.headNode
    var inner = f(outer.data).headNode
    while (outer.notDummy && inner.isDummy) {
      outer = outer.next
      inner = f(outer.data).headNode
    }
    ofNode(new FlatMappedSeqNode(outer, inner))
  }

  def cartesianProduct[U](that: Seq[U]): Seq[(T, U)] = this.flatMap(t => that.map(u => (t, u)))

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
    var nextNode = self.headNode
    while (nextNode.notDummy && !f(nextNode.data)) nextNode = nextNode.next
    ofNode(new FilteredSeqNode(nextNode))
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
    ofNode(new ConcatenatedSeqNode(true, headNode))
  }

  override def prepend[U >: T](x: U): Seq[U] = {
    val prependedNode: SeqNode[U] = new SeqNode[U] {
      def isDummy = false
      def data = x
      def next = self.headNode
    }
    ofNode(prependedNode)
  }

  override def append[U >: T](x: U): Seq[U] = {
    class AppendedSeqNode(val outer: SeqNode[U], val lastPassed: Boolean) extends SeqNode[U] {
      def isDummy = outer.isDummy && lastPassed
      def data = if (outer.notDummy) outer.data else x
      def next = if (outer.isDummy) new AppendedSeqNode(outer, true) else new AppendedSeqNode(outer.next, false)
    }
    ofNode(new AppendedSeqNode(self.headNode, false))
  }

  override def scanLeft[U](z: U)(f: (U, T) => U): Seq[U] = {
    class ScannedNode(val outer: SeqNode[T], val data: U) extends SeqNode[U] {
      def next = {
        if (outer.next.notDummy) new ScannedNode(outer.next, f(data, outer.next.data))
        else SeqNode.dummy
      }
      def isDummy = false
    }
    ofNode { new SeqNode[U] {
      def data = z
      def next = {
        if (self.headNode.notDummy) new ScannedNode(self.headNode, f(z, self.headNode.data))
        else SeqNode.dummy
      }
      def isDummy = false
    }}
  }

  override def scanByMonoid[U >: T : Monoid] = scanLeft(id[U])(_ op _)

  override def consecutive[U](f: (T, T) => U): Seq[U] = {
    class DiffNode(val a: SeqNode[T], val b: SeqNode[T]) extends SeqNode[U] {
      def data = f(b.data, a.data)
      def next = new DiffNode(b, b.next)
      def isDummy = b.isDummy
    }
    if (self.headNode.isDummy) Seq.empty
    else ofNode(new DiffNode(self.headNode, self.headNode.next))
  }

  override def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) => U.op(y, U.inv(x)))

  override def head = headNode.data

  override def tail: Seq[T] = ofNode(headNode.next)

  /**
    * Returns the list of tails of this sequence. $LAZY
    * @example {{{(1, 2, 3).tails == ((1, 2, 3), (2, 3), (3), ())}}}
    */
  override def tails = {
    class TailsNode(val outer: SeqNode[T], val isDummy: Boolean) extends SeqNode[Seq[T]] {
      def data = ofNode(outer)
      def next = new TailsNode(outer.next, outer.isDummy)
    }
    ofNode(new TailsNode(self.headNode, false))
  }

  override def init = {
    class InitNode(val n0: SeqNode[T], val n1: SeqNode[T]) extends SeqNode[T] {
      def data = n0.data
      def next = new InitNode(n1, n1.next)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofNode(new InitNode(self.headNode, self.headNode.next))
  }

  override def skip(n: Int) = {
    var node = self.headNode
    var i = 0
    while (node.notDummy && i < n) {
      node = node.next
      i += 1
    }
    ofNode(node)
  }

  override def skipWhile(f: T => Boolean) = {
    var node = self.headNode
    while (node.notDummy && f(node.data))
      node = node.next
    ofNode(node)
  }

  override def take(n: Int) = {
    class TakenNode(val i: Int, val outer: SeqNode[T]) extends SeqNode[T] {
      def data = outer.data
      def next = new TakenNode(i + 1, outer.next)
      def isDummy = outer.isDummy || i >= n
    }
    ofNode(new TakenNode(0, self.headNode))
  }

  override def takeWhile(f: T => Boolean) = {
    class TakenWhileNode(val outer: SeqNode[T]) extends SeqNode[T] {
      def isDummy = outer.isDummy || !f(outer.data)
      def data = outer.data
      def next = if (!f(outer.next.data)) SeqNode.dummy else new TakenWhileNode(outer.next)
    }
    ofNode(new TakenWhileNode(self.headNode))
  }

  override def takeTo(f: T => Boolean) = {
    class TakenToNode(val outer: SeqNode[T]) extends SeqNode[T] {
      def data = outer.data
      def next = if (f(outer.data)) SeqNode.dummy else new TakenToNode(outer.next)
      def isDummy = outer.isDummy
    }
    ofNode(new TakenToNode(self.headNode))
  }

  override def takeUntil(f: T => Boolean) = takeWhile(x => !f(x))

  override def slice(i: Int, j: Int) = self.skip(i).take(j - i)

  override def rotate(i: Int) = self.skip(i) ++ self.take(i)

  /**
   * Pretends that this sequence is sorted under the given implicit order.
   * @param U The implicit order
   * @note Actual orderedness is not guaranteed! The user should make sure that it is sorted.
   * @return A sorted order
   */
  override def asIfSorted[U >: T](implicit U: WeakOrder[U]): SortedSeq[U] = new SortedSeq[U] {
    val orderOnValue: WeakOrder[U] = U
    def headNode: SeqNode[T] = self.headNode
  }

  def zip[U](that: Seq[U]): Seq[(T, U)] = ofNode(self.headNode zip that.headNode)

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


  def asSeq: Seq[T] = ofNode(headNode)

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Equiv[T].eq(this, that)
    case _ => false
  }

  override def toString = "(" + buildString(",") + ")" // overridden the `toString` in Map

  override def hashCode = ???

  override def |>[U](f: T => U): Seq[U] = ofNode(self.headNode.map(f))
  override def +:[U >: T](u: U): Seq[U] = this prepend u
  override def :+[U >: T](u: U): Seq[U] = this append u
  def ++[U >: T](that: Seq[U]) = this concat that
}

object Seq {

  def unapply[T](xs: Seq[T]) = {
    if (xs.isEmpty) None
    else Some((xs.head, xs.tail))
  }

  object empty extends Seq[Nothing] {
    def headNode = SeqNode.dummy
  }

  def ofNode[T](node: SeqNode[T]): Seq[T] = new AbstractSeq[T] {
    def headNode = node
  }

  def iterate[T](s: T)(f: T => T): Seq[T] = {
    class IteratedSeqNode(val data: T) extends SeqNode[T] {
      def next = new IteratedSeqNode(f(data))
      def isDummy = false
    }
    ofNode(new IteratedSeqNode(s))
  }

  def infinite[T](x: => T): Seq[T] = {
    class InfiniteSeqNode extends SeqNode[T] {
      def next = this
      def data = x
      def isDummy = false
    }
    ofNode(new InfiniteSeqNode)
  }

  implicit def Equiv[T: Equiv]: Equiv[Seq[T]] = new Equiv[Seq[T]] {
    def eq(x: Seq[T], y: Seq[T]): Boolean = {
      val xi = x.newIterator
      val yi = y.newIterator
      while (xi.advance() && yi.advance())
        if (xi.current =!= yi.current) return false
      if (xi.advance()) return false
      if (yi.advance()) return false
      true
    }
  }
}

abstract class AbstractSeq[+T] extends AbstractIterable[T] with Seq[T]
