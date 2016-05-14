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
trait BiSeq[+T] extends Seq[T] with BiIterable[T] { self =>

  import BiSeq._

  /** Returns a dummy node whose next node is the head of this sequence,
    * and whose previous node is the last of this sequence.
    */
  override def dummy: BiSeqNode[T] = new BiSeqNode[T] {
    def prev = lastNode
    def next = headNode
    def data = throw new DummyNodeException
    def isDummy = true
  }

  def newReverseIterator = reverse.newIterator

  def headNode: BiSeqNode[T]

  /** Returns the last node of this sequence. */
  def lastNode: BiSeqNode[T]

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = ofDummyNode(dummy map f)

  override def consecutive[U](f: (T, T) => U): BiSeq[U] = {
    class ConsecutiveNode(val n0: BiSeqNode[T], val n1: BiSeqNode[T]) extends BiSeqNode[U] {
      def data = f(n1.data, n0.data)
      def next = new ConsecutiveNode(n1, n1.next)
      def prev = new ConsecutiveNode(n0.prev, n0)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofDummyNode {
      new BiSeqNode[U] {
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
    class From(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadAndLastNode(n, self.lastNode)
      def next = new From(n.next)
      def prev = new From(n.prev)
      def isDummy = n.isDummy
    }
    ofHeadAndLastNode(new From(self.headNode), new From(self.lastNode))
  }

  override def prefixes = {
    class Until(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadAndLastNode(n.reverse, self.headNode.reverse)
      def next = new Until(n.prev)
      def prev = new Until(n.next)
      def isDummy = n.isDummy
    }
    ofHeadAndLastNode(new Until(self.lastNode), new Until(self.headNode))
  }

  override def reverse: BiSeq[T] = new AbstractBiSeq[T] {
    def headNode = self.lastNode.reverse
    def lastNode = self.headNode.reverse
    override def reverse = self
  }

  def asBiSeq: BiSeq[T] = new AbstractBiSeq[T] {
    def headNode = self.headNode
    def lastNode = self.lastNode
  }

}

object BiSeq {

  def ofDummyNode[T](d: BiSeqNode[T]): BiSeq[T] = new AbstractBiSeq[T] {
    override def dummy = d
    def headNode = d.next
    def lastNode = d.prev
  }

  def ofHeadAndLastNode[T](hn: BiSeqNode[T], ln: BiSeqNode[T]): BiSeq[T] = new AbstractBiSeq[T] {
    def headNode = hn
    def lastNode = ln
  }

  object empty extends BiSeq[Nothing] {
    override def dummy: BiSeqNode[Nothing] = BiSeqNode.dummy
    def headNode = dummy
    def lastNode = dummy
  }

}

abstract class AbstractBiSeq[+T] extends AbstractSeq[T] with BiSeq[T]
