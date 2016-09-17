package poly.collection

import poly.collection.exception._
import poly.collection.node._

/**
 * Represents a bidirectional sequence, i.e. a sequence that supports
 * fast access to the last element as well as fast reversed traversal.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiSeq[+T] extends Seq[T] with BidiIterable[T] { self =>

  import BidiSeq._

  /**
   * Returns a dummy node whose next node is the head of this sequence,
   * and whose previous node is the last of this sequence.
   */
  override def dummy: BidiSeqNode[T] = new BidiSeqNode[T] {
    def prev = lastNode
    def next = headNode
    def data = throw new DummyNodeException
    def isDummy = true
  }

  override def apply(i: Int): T = {
    if (i >= 0) {
      var node = headNode
      var j = 0
      while (j < i) {
        node = node.next
        j += 1
      }
      node.data
    }
    else { // negative indices
      var node = lastNode
      var j = -1
      while (j > i) {
        node = node.prev
        j -= 1
      }
      node.data
    }
  }

  def newReverseIterator = reverse.newIterator

  def headNode: BidiSeqNode[T]

  /** Returns the last node of this sequence. */
  def lastNode: BidiSeqNode[T]

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BidiSeq[U] = {
    class MappedNode(outer: BidiSeqNode[T]) extends BidiSeqNode[U] {
      def prev = new MappedNode(outer.prev)
      def next = new MappedNode(outer.next)
      def data = f(outer.data)
      def isDummy = outer.isDummy
    }
    ofDummyNode(new MappedNode(self.dummy))
  }

  override def consecutive[U](f: (T, T) => U): BidiSeq[U] = {
    class ConsecutiveNode(val n0: BidiSeqNode[T], val n1: BidiSeqNode[T]) extends BidiSeqNode[U] {
      def data = f(n1.data, n0.data)
      def next = new ConsecutiveNode(n1, n1.next)
      def prev = new ConsecutiveNode(n0.prev, n0)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofDummyNode {
      new BidiSeqNode[U] {
        def data = throw new DummyNodeException
        def next = new ConsecutiveNode(self.headNode, self.headNode.next)
        def prev = new ConsecutiveNode(self.lastNode.prev, self.lastNode)
        def isDummy = true
      }
    }
  }

  override def tail = ofHeadAndLastNode(headNode.next, lastNode)

  override def init = ofHeadAndLastNode(headNode, lastNode.prev)

  override def last = lastNode.data

  override def suffixes = {
    class From(val n: BidiSeqNode[T]) extends BidiSeqNode[BidiSeq[T]] {
      def data = ofHeadAndLastNode(n, self.lastNode)
      def next = new From(n.next)
      def prev = new From(n.prev)
      def isDummy = n.isDummy
    }
    ofHeadAndLastNode(new From(self.headNode), new From(self.lastNode))
  }

  override def prefixes = {
    class Until(val n: BidiSeqNode[T]) extends BidiSeqNode[BidiSeq[T]] {
      def data = ofHeadAndLastNode(n.reverse, self.headNode.reverse)
      def next = new Until(n.prev)
      def prev = new Until(n.next)
      def isDummy = n.isDummy
    }
    ofHeadAndLastNode(new Until(self.lastNode), new Until(self.headNode))
  }

  override def reverse: BidiSeq[T] = new BidiSeqT.Reversed(self)

  def asBiSeq: BidiSeq[T] = new AbstractBidiSeq[T] {
    def headNode = self.headNode
    def lastNode = self.lastNode
  }

}

object BidiSeq {

  def ofDummyNode[T](d: BidiSeqNode[T]): BidiSeq[T] = new AbstractBidiSeq[T] {
    override def dummy = d
    def headNode = d.next
    def lastNode = d.prev
  }

  def ofHeadAndLastNode[T](hn: BidiSeqNode[T], ln: BidiSeqNode[T]): BidiSeq[T] = new AbstractBidiSeq[T] {
    def headNode = hn
    def lastNode = ln
  }

  object empty extends BidiSeq[Nothing] {
    override def dummy: BidiSeqNode[Nothing] = BidiSeqNode.dummy
    def headNode = dummy
    def lastNode = dummy
  }

}

abstract class AbstractBidiSeq[+T] extends AbstractSeq[T] with BidiSeq[T]

private[poly] object BidiSeqT {

  class Reversed[T](self: BidiSeq[T]) extends AbstractBidiSeq[T] {
    def headNode = self.lastNode.reverse
    def lastNode = self.headNode.reverse
    override def reverse = self
  }

}