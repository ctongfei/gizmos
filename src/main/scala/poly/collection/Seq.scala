package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.implicits._
import poly.collection.node._

/**
 * Represents a sequence that guarantees the same order every time it is traversed.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Seq[+T] extends Iterable[T] with Map[Int, T] { self =>

  import Seq._

  def headNode: SeqNode[T]

  /**
   * Returns the length of this sequence.
   * @return The length of this sequence
   */
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
   * Returns the ''i''-th element of this sequence.
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

  def equivOnKey = TotalOrder[Int]

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

  def pairs = {
    var i = -1
    self.map(x => { i += 1; (i, x) })
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
        if (innerNext.isDummy) {
          var newOuter = outer
          var newInner = f(newOuter.data).headNode
          while (newOuter.notDummy) {
            newOuter = newOuter.next
            newInner = f(newOuter.data).headNode
            if (newInner.notDummy) return new FlatMappedSeqNode(newOuter, newInner)
          }
          new FlatMappedSeqNode(newOuter, newInner) // iteration complete, no more elements
        }
        else new FlatMappedSeqNode(outer, innerNext)
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

  override def filter(f: T => Boolean): Seq[T] = {
    class FilteredSeqNode(val node: SeqNode[T]) extends SeqNode[T] {
      override def isDummy = node.isDummy
      def data = node.data
      def next = {
        var nn = node.next
        while (nn.notDummy && !f(nn.data)) nn = nn.next
        new FilteredSeqNode(nn)
      }
    }
    var nn = self.headNode
    while (nn.notDummy && !f(nn.data)) nn = nn.next
    ofNode(new FilteredSeqNode(nn))
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
      override def isDummy = outer.isDummy && lastPassed
      def data = if (outer.notDummy) outer.data else x
      def next = if (outer.isDummy) new AppendedSeqNode(outer, true) else new AppendedSeqNode(outer.next, false)
    }
    ofNode(new AppendedSeqNode(self.headNode, false))
  }

  override def head = headNode.data

  override def tail: Seq[T] = ofNode(headNode.next)

  override def foldRight[U](z: U)(f: (T, U) => U): U = { //TODO: change to non-recursion with an ArrayStack
    if (headNode.isDummy) z
    else f(head, tail.foldRight(z)(f))
  }

  override def takeWhile(f: T => Boolean) = {
    class TakenWhileNode(val outer: SeqNode[T]) extends SeqNode[T] {
      def data = outer.data
      def next = if (!f(outer.data)) SeqNode.dummy else new TakenWhileNode(outer.next)
      def isDummy = outer.isDummy
    }
    ofNode(new TakenWhileNode(self.headNode))
  }

  override def takeUntil(f: T => Boolean) = takeWhile(x => !f(x))

  /**
   * Pretends that this sequence is sorted under the given implicit order.
   * @param U The implicit order
   * @return A sorted order (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   */
  override def asIfSorted[U >: T](implicit U: WeakOrder[U]): SortedSeq[U] = new SortedSeq[U] {
    val orderOnKey: WeakOrder[U] = U
    def headNode: SeqNode[T] = self.headNode
  }

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Equiv[T].eq(this, that)
    case _ => false
  }

  override def toString = "(" + buildString(",") + ")" // overridden the `toString` in Map

  override def hashCode = ???


  override def |>[U](f: T => U): Seq[U] = ofNode(self.headNode.map(f))

  def asIterable = Iterable.ofIterator(self.newIterator)

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
