package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has exactly one predecessor and one successor.
 * It is the type of nodes in a bidirectional sequence ([[poly.collection.BidiSeq]]).
 * @since 0.1.0
 */
trait BidiSeqNodeLike[+T, +N <: BidiSeqNodeLike[T, N]] extends BidiNodeLike[T, N] with SeqNodeLike[T, N] with NodeWithParentLike[T, N] { self: N =>
  def prev: N
  def next: N

  override def parent: N = prev
  override def pred: Iterable[N] = if (prev.isDummy) Iterable.Empty else Iterable.single(prev)
  override def succ: Iterable[N] = if (next.isDummy) Iterable.Empty else Iterable.single(next)

}

trait BidiSeqNode[+T] extends BidiNode[T] with SeqNode[T] with NodeWithParent[T] with BidiSeqNodeLike[T, BidiSeqNode[T]] { self =>

  override def reverse: BidiSeqNode[T] = new BidiSeqNode[T] {
    def next = self.prev.reverse
    def prev = self.next.reverse
    def data = self.data
    override def reverse = self
    def isDummy = self.isDummy
  }
}

object BidiSeqNode {
  object dummy extends BidiSeqNode[Nothing] {
    def data = throw new NoSuchElementException
    def next = dummy
    def prev = dummy
    def isDummy = true
  }
}
