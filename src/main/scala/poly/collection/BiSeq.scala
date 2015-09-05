package poly.collection

import poly.collection.exception._
import poly.collection.node._

/**
 * Represents a bidirectional sequence, i.e. a sequence that supports
 * fast access to the last element as well as fast reversed traversal.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiSeq[+T] extends Seq[T] { self =>

  def headNode: BiSeqNode[T]
  def lastNode: BiSeqNode[T]


  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = new AbstractBiSeq[U] {
    def headNode = self.headNode map f
    def lastNode = self.lastNode map f
    def length = self.length
    def apply(i: Int) = f(self(i))
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

  /** Retrieves the last element of this sequence. $EAGER $CX_1 */
  override def last = lastNode.data

  /** Returns the reverse of this bidirectional sequence. $LAZY $CX_1 */
  override def reverse: BiSeq[T] = new AbstractBiSeq[T] {
    def lastNode = self.headNode.reverse
    def headNode = self.lastNode.reverse
    def length = self.length
    def apply(i: Int) = self.apply(self.length - 1 - i)
    override def reverse = self
  }

}

object BiSeq {

  object empty extends BiSeq[Nothing] {
    def headNode: BiSeqNode[Nothing] = throw new NoSuchElementException
    def lastNode: BiSeqNode[Nothing] = throw new NoSuchElementException
    def length: Int = 0
    def apply(i: Int): Nothing = throw new NoSuchElementException
  }

}

abstract class AbstractBiSeq[+T] extends AbstractSeq[T] with BiSeq[T]
