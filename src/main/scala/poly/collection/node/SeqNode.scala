package poly.collection.node

import poly.collection._
import poly.collection.exception._
import poly.collection.mut._

/**
 * Represents a node that has only one successor node.
 * It is the type of nodes in a typical sequence ([[poly.collection.Seq]]).
 * @since 0.1.0
 */
trait SeqNode[+T] extends ForwardNode[T] { self =>
  def data: T
  def next: SeqNode[T]
  def succ: Iterable[SeqNode[T]] = ListSeq(next).filter(_.notDummy)


  override def reverse: NodeWithParent[T] = new NodeWithParent[T] {
    def data = self.data
    def parent = self.next.reverse
    def isDummy = self.isDummy
    override def reverse = self
  }

  override def map[U](f: T => U): SeqNode[U] = new SeqNode[U] {
    def next = self.next.map(f)
    def data = f(self.data)
    def isDummy = self.isDummy
  }

  def zip[U](that: SeqNode[U]): SeqNode[(T, U)] = new SeqNode[(T, U)] {
    def data = (self.data, that.data)
    def next = self.next zip that.next
    def isDummy = self.isDummy || that.isDummy
  }

}

object SeqNode {

  object dummy extends SeqNode[Nothing] {
    def isDummy = true
    def data = throw new NoSuchElementException
    def next = this
  }

}