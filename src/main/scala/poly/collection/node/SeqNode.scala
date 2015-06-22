package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has only one successor node.
 * @since 0.1.0
 */
trait SeqNode[+T] extends Node[T] { self =>
  def next: SeqNode[T]
  def succ: Enumerable[SeqNode[T]] = if (next eq null) ListSeq() else ListSeq(next)
  def map[U](f: T => U): SeqNode[U] = new SeqNode[U] {
    def next = self.next.map(f)
    def data = f(self.data)
  }
}

/**
 * Represents a node that has exactly one predecessor and one successor.
 * @since 0.1.0
 */
trait BidiSeqNode[+T] extends BidiNode[T] with SeqNode[T] with SinglePredNode[T] { self =>

  def prev: BidiSeqNode[T]
  def next: BidiSeqNode[T]
  override def parent: BidiSeqNode[T] = prev
  override def pred: Enumerable[BidiSeqNode[T]] = if (prev eq null) ListSeq() else ListSeq(prev)
  override def succ: Enumerable[BidiSeqNode[T]] = if (next eq null) ListSeq() else ListSeq(next)
  override def map[U](f: T => U): BidiSeqNode[U] = new BidiSeqNode[U] {
    def prev = self.prev.map(f)
    def next = self.next.map(f)
    def data = f(self.data)
  }
}
