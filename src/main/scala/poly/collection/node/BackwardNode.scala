package poly.collection.node

import poly.collection._

/**
 * Represents a node that has a list of predecessor nodes.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait BackwardNodeLike[+T, +N <: BackwardNodeLike[T, N]] extends NodeLike[T, N] { self: N =>

  /** Returns the list of predecessor nodes of this node. */
  def pred: Iterable[N]

  override def toString = s"[${pred map {_.data}} → $dataString]"
}

trait BackwardNode[+T] extends Node[T] with BackwardNodeLike[T, BackwardNode[T]] { self =>

  def map[U](f: T => U): BackwardNode[U] = new BackwardNode[U] {
    def pred = self.pred.map(_.map(f))
    def data = f(self.data)
    def isDummy = self.isDummy
  }

}
