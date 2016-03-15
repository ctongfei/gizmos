package poly.collection

import poly.collection.exception._
import poly.collection.node._

/**
 * Represents a bidirectional sequence, i.e. a sequence that supports
 * fast access to the last element as well as fast reversed traversal.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiSeq[+T] extends Seq[T] { self =>

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

  def headNode: BiSeqNode[T]

  def lastNode: BiSeqNode[T]

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = ofDummyNode(dummy map f)

  override def foldRight[U](z: U)(f: (T, U) => U): U = {
    var accum = z
    var node = dummy.prev
    while (node.notDummy) {
      accum = f(node.data, accum)
      node = node.prev
    }
    accum
  }

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
        def next = new ConsecutiveNode(self.dummy.next, self.dummy.next.next)
        def prev = new ConsecutiveNode(self.dummy.prev.prev, self.dummy.prev)
        def isDummy = true
      }
    }
  }

  override def tail = ofHeadLastNode(dummy.next.next, dummy.prev)

  override def init = ofHeadLastNode(dummy.next, dummy.prev.prev)

  override def last = dummy.prev.data

  override def suffixes = {
    class From(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadLastNode(n, self.dummy.prev)
      def next = new From(n.next)
      def prev = new From(n.prev)
      def isDummy = n.isDummy
    }
    ofHeadLastNode(new From(self.dummy.next), new From(self.dummy.prev))
  }

  override def prefixes = {
    class Until(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadLastNode(n.reverse, self.dummy.next.reverse)
      def next = new Until(n.prev)
      def prev = new Until(n.next)
      def isDummy = n.isDummy
    }
    ofHeadLastNode(new Until(self.dummy.prev), new Until(self.dummy.next))
  }

  override def reverse: BiSeq[T] = new AbstractBiSeq[T] {
    def headNode = self.lastNode.reverse
    def lastNode = self.headNode.reverse
    override def reverse = self
  }

  override def rotate(n: Int): BiSeq[T] = ???

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

  def ofHeadLastNode[T](hn: BiSeqNode[T], ln: BiSeqNode[T]): BiSeq[T] = new AbstractBiSeq[T] {
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
