package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiSeq[+T] extends Seq[T] { self =>

  def headNode: BiSeqNode[T]
  def lastNode: BiSeqNode[T]

  //TODO: should override basic reverse in Seq/Enumerable
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
