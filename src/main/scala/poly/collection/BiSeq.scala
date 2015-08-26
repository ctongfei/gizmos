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

  override def last = lastNode.data

  //region HELPER FUNCTIONS

  override def map[U](f: T => U): BiSeq[U] = new BiSeq[U] {
    def headNode = self.headNode.map(f)
    def lastNode = self.lastNode.map(f)
    def length = self.length
    def apply(i: Int) = f(self(i))
  }

  /**
   * Returns the reverse of this bidirectional sequence.
   * O(1) complexity. $LAZY
   * @return The reversed sequence.
   */
  override def reverse: BiSeq[T] = new BiSeq[T] {
    def lastNode = self.headNode
    def headNode = self.lastNode
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