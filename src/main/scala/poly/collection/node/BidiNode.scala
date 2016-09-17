package poly.collection.node

import poly.collection._

/**
 * Represents a node that has a list of successor nodes as well as a list of predecessor nodes.
 * @since 0.1.0
 */
trait BidiNodeLike[+T, +N <: BidiNodeLike[T, N]] extends ForwardNodeLike[T, N] with BackwardNodeLike[T, N] { self: N =>

  def pred: Iterable[N]
  def succ: Iterable[N]

  override def toString = s"[${pred map {_.data}} → $dataString → ${succ map {_.data}}]"

}


trait BidiNode[+T] extends ForwardNode[T] with BackwardNode[T] with BidiNodeLike[T, BidiNode[T]] { self =>

  def reverse: BidiNode[T] = new BidiNode[T] {
    def data = self.data
    def pred = self.succ.map(_.reverse)
    def succ = self.pred.map(_.reverse)
    override def reverse = self
    def isDummy = self.isDummy
  }


}
