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

  /**
   * Returns the reverse of this bidirectional sequence.
   * O(1) complexity. $LAZY
   * @return The reversed sequence.
   */
  def reverse: BiSeq[T] = new BiSeq[T] {
    def lastNode: BiSeqNode[T] = self.headNode
    def headNode: BiSeqNode[T] = self.lastNode
    def length: Int = self.length
    def apply(i: Int): T = self.apply(self.length - 1 - i)
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