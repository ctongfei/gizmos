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
trait BiSeq[+T] extends Seq[T] { self =>

  import BiSeq._

  /** Returns a dummy node whose next node is the head of this sequence,
    * and whose previous node is the last of this sequence.
    */
  def dummy: BiSeqNode[T]

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = ofNode(dummy map f)

  override def foldRight[U](z: U)(f: (T, U) => U): U = {
    var accum = z
    var node = dummy.prev
    while (node.notDummy) {
      accum = f(node.data, accum)
      node = node.prev
    }
    accum
  }

  def concat[U >: T](that: BiSeq[U]): BiSeq[U] = {
    ???
  }

  override def consecutive[U](f: (T, T) => U): BiSeq[U] = {
    class ConsecutiveNode(val n0: BiSeqNode[T], val n1: BiSeqNode[T]) extends BiSeqNode[U] {
      def data = f(n1.data, n0.data)
      def next = new ConsecutiveNode(n1, n1.next)
      def prev = new ConsecutiveNode(n0.prev, n0)
      def isDummy = n0.isDummy || n1.isDummy
    }
    ofNode {
      new BiSeqNode[U] {
        def data = throw new DummyNodeException
        def next = new ConsecutiveNode(self.dummy.next, self.dummy.next.next)
        def prev = new ConsecutiveNode(self.dummy.prev.prev, self.dummy.prev)
        def isDummy = true
      }
    }
  }

  override def tail = ofHeadNode(dummy.next.next, dummy.prev)

  override def init = ofHeadNode(dummy.next, dummy.prev.prev)

  override def last = dummy.prev.data

  override def suffixes = {
    class From(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadNode(n, self.dummy.prev)
      def next = new From(n.next)
      def prev = new From(n.prev)
      def isDummy = n.isDummy
    }
    ofHeadNode(new From(self.dummy.next), new From(self.dummy.prev))
  }

  override def prefixes = {
    class Until(val n: BiSeqNode[T]) extends BiSeqNode[BiSeq[T]] {
      def data = ofHeadNode(n.reverse, self.dummy.next.reverse)
      def next = new Until(n.prev)
      def prev = new Until(n.next)
      def isDummy = n.isDummy
    }
    ofHeadNode(new Until(self.dummy.prev), new Until(self.dummy.next))
  }

  override def reverse: BiSeq[T] = new AbstractBiSeq[T] {
    def dummy = self.dummy.reverse
    override def reverse = self
  }

  override def rotate(n: Int): BiSeq[T] = ???

  def asBiSeq: BiSeq[T] = new AbstractBiSeq[T] {
    def dummy = self.dummy
  }

}

object BiSeq {

  def ofNode[T](d: BiSeqNode[T]): BiSeq[T] = new AbstractBiSeq[T] {
    def dummy = d
  }

  def ofHeadNode[T](hn: BiSeqNode[T], ln: BiSeqNode[T]): BiSeq[T] = new AbstractBiSeq[T] {
    def dummy =
      new BiSeqNode[T] {
        def next = hn
        def prev = ln
        def data = throw new DummyNodeException
        def isDummy = true
      }
  }


  object empty extends BiSeq[Nothing] {
    def dummy: BiSeqNode[Nothing] = BiSeqNode.dummy
  }

}

abstract class AbstractBiSeq[+T] extends AbstractSeq[T] with BiSeq[T]
