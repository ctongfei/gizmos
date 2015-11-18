package poly.collection

import poly.collection.exception._
import poly.collection.node._

/**
 * Represents a bidirectional sequence, i.e. a sequence that supports
 * fast access to the last element as well as fast reversed traversal.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait BiSeq[+T] extends Seq[T] { self =>

  import BiSeq._

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
        def data = throw new NoSuchElementException
        def isDummy = true
      }
  }


  object empty extends BiSeq[Nothing] {
    def dummy: BiSeqNode[Nothing] = BiSeqNode.dummy
  }

}

abstract class AbstractBiSeq[+T] extends AbstractSeq[T] with BiSeq[T]
