package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait LinearSeq[+T] extends Seq[T] { self =>

  import LinearSeq._

  /**
   * Returns the length of this sequence.
   * @return The length of this sequence
   */
  override def length: Int = {
    var node = headNode
    var n = 0
    while (node.notDummy) {
      node = node.next
      n += 1
    }
    n
  }

  /**
   * Returns the ''i''-th element of this sequence.
   * @param i Index
   * @return The ''i''-th element of this sequence
   */
  override def apply(i: Int): T = {
    var node = headNode
    var j = 0
    while (j < i) {
      node = node.next
      j += 1
    }
    node.data
  }

  // HELPER FUNCTIONS

  override def map[U](f: T => U): LinearSeq[U] = new AbstractLinearSeq[U] {
    def headNode: SeqNode[U] = self.headNode.map(f)
  }

  override def filter(f: T => Boolean): LinearSeq[T] = {
    class FilteredSeqNode(val node: SeqNode[T]) extends SeqNode[T] {
      override def isDummy = node.isDummy
      def data = node.data
      def next = {
        var nn = node.next
        while (nn.notDummy && !f(nn.data)) nn = nn.next
        new FilteredSeqNode(nn)
      }
    }
    var nn = self.headNode
    while (nn.notDummy && !f(nn.data)) nn = nn.next
    ofNode(new FilteredSeqNode(nn))
  }

  override def foldRight[U](z: U)(f: (T, U) => U): U = {
    if (headNode.isDummy) z
    else f(head, tail.foldRight(z)(f))
  }

}

object LinearSeq {
  def ofNode[T](node: => SeqNode[T]): LinearSeq[T] = new AbstractLinearSeq[T] {
    def headNode = node
  }
}

abstract class AbstractLinearSeq[+T] extends AbstractSeq[T] with LinearSeq[T]
