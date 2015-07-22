package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has only one successor node.
 * It is the type of nodes in a sequence ([[poly.collection.Seq]]).
 * @since 0.1.0
 */
trait SeqNode[+T] extends Node[T] { self =>
  def next: SeqNode[T]
  def succ: Enumerable[SeqNode[T]] = ListSeq.applyNotNull(next)
  override def map[U](f: T => U): SeqNode[U] = new SeqNode[U] {
    def next = self.next.map(f)
    def data = f(self.data)
  }
}

/**
 * Represents a node that has exactly one predecessor and one successor.
 * It is the type of nodes in a bidirectional sequence ([[poly.collection.BiSeq]]).
 * @since 0.1.0
 */
trait BiSeqNode[+T] extends BiNode[T] with SeqNode[T] with SinglePredNode[T] { self =>

  def prev: BiSeqNode[T]
  def next: BiSeqNode[T]
  override def parent: BiSeqNode[T] = prev
  override def pred: Enumerable[BiSeqNode[T]] = ListSeq.applyNotNull(prev)
  override def succ: Enumerable[BiSeqNode[T]] = ListSeq.applyNotNull(next)
  override def map[U](f: T => U): BiSeqNode[U] = new BiSeqNode[U] {
    def prev = self.prev.map(f)
    def next = self.next.map(f)
    def data = f(self.data)
  }
}
