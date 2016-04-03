package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has exactly one predecessor and one successor.
 * It is the type of nodes in a bidirectional sequence ([[poly.collection.BiSeq]]).
 * @since 0.1.0
 */
trait BiSeqNodeLike[+T, +N <: BiSeqNodeLike[T, N]] extends BiNodeLike[T, N] with SeqNodeLike[T, N] with NodeWithParentLike[T, N] { self: N =>
  def prev: N
  def next: N

  override def parent: N = prev
  override def pred: Iterable[N] = if (isDummy) Iterable.empty else Iterable.single(prev)
  override def succ: Iterable[N] = if (isDummy) Iterable.empty else Iterable.single(next)

}

trait BiSeqNode[+T] extends BiNode[T] with SeqNode[T] with NodeWithParent[T] with BiSeqNodeLike[T, BiSeqNode[T]] { self =>

  override def reverse: BiSeqNode[T] = new BiSeqNode[T] {
    def next = self.prev.reverse
    def prev = self.next.reverse
    def data = self.data
    override def reverse = self
    def isDummy = self.isDummy
  }
  override def map[U](f: T => U): BiSeqNode[U] = new BiSeqNode[U] {
    def isDummy = self.isDummy
    def prev = self.prev map f
    def next = self.next map f
    def data = f(self.data)
  }
}

object BiSeqNode {
  object dummy extends BiSeqNode[Nothing] {
    def data = throw new NoSuchElementException
    def next = dummy
    def prev = dummy
    def isDummy = true
  }
}
