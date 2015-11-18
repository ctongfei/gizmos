package poly.collection.node

import poly.collection._
import poly.collection.mut._
import poly.collection.ops._

/**
 * Represents a node that has only one predecessor node.
 * @since 0.1.0
 */
trait NodeWithParent[+T] extends BackwardNode[T] { self =>
  def data: T
  def parent: NodeWithParent[T]
  def pred: Iterable[NodeWithParent[T]] = ListSeq(parent).filter(_.notDummy)

  override def map[U](f: T => U): NodeWithParent[U] = new NodeWithParent[U] {
    def parent = self.parent.map(f)
    def data = f(self.data)
    override def isDummy = self.isDummy
  }

  /** Backtracks from this node to the initial node. */
  def pathToRoot = this.iterate(_.parent).takeUntil(_.isDummy)
}
