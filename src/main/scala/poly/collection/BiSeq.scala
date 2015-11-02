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

  def headNode: BiSeqNode[T]
  def lastNode: BiSeqNode[T]

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = new AbstractBiSeq[U] {
    def headNode = self.headNode map f
    def lastNode = self.lastNode map f
  }

  override def foldRight[U](z: U)(f: (T, U) => U): U = {
    var accum = z
    var node = lastNode
    while (node.notDummy) {
      accum = f(node.data, accum)
      node = node.prev
    }
    accum
  }

  /** Retrieves the last element of this sequence. $EAGER $O1 */
  override def last = lastNode.data

  /** Returns the reverse of this bidirectional sequence. $LAZY $O1 */
  override def reverse: BiSeq[T] = new AbstractBiSeq[T] {
    def lastNode = self.headNode.reverse
    def headNode = self.lastNode.reverse
    override def reverse = self
  }

}

object BiSeq {

  object empty extends BiSeq[Nothing] {
    def headNode: BiSeqNode[Nothing] = BiSeqNode.dummy
    def lastNode: BiSeqNode[Nothing] = BiSeqNode.dummy
  }

}

abstract class AbstractBiSeq[+T] extends AbstractSeq[T] with BiSeq[T]
