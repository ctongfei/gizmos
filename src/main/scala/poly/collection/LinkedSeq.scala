package poly.collection

import poly.collection.node._

import scala.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait LinkedSeq[+T] extends Seq[T] { self =>

  import LinkedSeq._

  /**
   * Returns the length of this sequence.
   * @return The length of this sequence
   */
  override def length: Int = {
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

  // HELPER FUNCTIONS

  override def map[U](f: T => U): LinkedSeq[U] = ofNode(self.headNode.map(f))

  override def flatMap[U](f: T => Seq[U]): LinkedSeq[U] = {
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

  override def filter(f: T => Boolean): LinkedSeq[T] = {
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

  override def concat[U >: T](that: Seq[U]): LinkedSeq[U] = {
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

  override def prepend[U >: T](x: U): LinkedSeq[U] = {
    val prependedNode: SeqNode[U] = new SeqNode[U] {
      def isDummy = false
      def data = x
      def next = self.headNode
    }
    ofNode(prependedNode)
  }

  override def append[U >: T](x: U): LinkedSeq[U] = {
    class AppendedSeqNode(val outer: SeqNode[U], val lastPassed: Boolean) extends SeqNode[U] {
      override def isDummy = outer.isDummy && lastPassed
      def data = if (outer.notDummy) outer.data else x
      def next = if (outer.isDummy) new AppendedSeqNode(outer, true) else new AppendedSeqNode(outer.next, false)
    }
    ofNode(new AppendedSeqNode(self.headNode, false))
  }

  override def tail: LinkedSeq[T] = ofNode(headNode.next)

  override def foldRight[U](z: U)(f: (T, U) => U): U = { //TODO: change to non-recursion with an ArrayStack
    if (headNode.isDummy) z
    else f(head, tail.foldRight(z)(f))
  }

}

object LinkedSeq {

  object empty extends LinkedSeq[Nothing] {
    def headNode = SeqNode.dummy
  }

  def ofNode[T](node: SeqNode[T]): LinkedSeq[T] = new AbstractLinkedSeq[T] {
    def headNode = node
  }

  def iterate[T](s: T)(f: T => T): LinkedSeq[T] = {
    class IteratedSeqNode(val data: T) extends SeqNode[T] {
      def next = new IteratedSeqNode(f(data))
      def isDummy = false
    }
    ofNode(new IteratedSeqNode(s))
  }
}

abstract class AbstractLinkedSeq[+T] extends AbstractSeq[T] with LinkedSeq[T]
