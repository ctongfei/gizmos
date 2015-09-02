package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has only one predecessor node.
 * @since 0.1.0
 */
trait SinglePredNode[+T] extends BackwardNode[T] { self =>
  def data: T
  def parent: SinglePredNode[T]
  def pred: Iterable[SinglePredNode[T]] = ListSeq.applyNotNull(parent)
  def map[U](f: T => U): SinglePredNode[U] = new SinglePredNode[U] {
    def parent = self.parent.map(f)
    def data = f(self.data)
    override def isDummy = self.isDummy
  }

  override def reverse: SeqNode[T] = new SeqNode[T] {
    def data = self.data
    def next = self.parent.reverse
    override def reverse = self
    override def isDummy = self.isDummy
  }

  /** Backtracks from this node to the initial node. */
  def pathToRoot = Iterable.iterate(this)(_.parent).takeUntil(_.isDummy)
}
