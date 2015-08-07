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
  def succ: Enumerable[SeqNode[T]] = ListSeq(next).filter(_.notDummy)

  def isDummy = false
  def notDummy = !isDummy

  override def map[U](f: T => U): SeqNode[U] = new SeqNode[U] {
    def next = self.next.map(f)
    def data = f(self.data)
  }
}


