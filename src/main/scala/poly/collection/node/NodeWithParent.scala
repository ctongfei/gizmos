package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has only one predecessor node.
 * @since 0.1.0
 */
trait NodeWithParentLike[+T, +N <: NodeWithParentLike[T, N]] extends BackwardNodeLike[T, N] { self: N ⇒
  /** Gets the unique parent node of this node. */
  def parent: N

  def pred: Iterable[N] = ListSeq(parent).filter(_.notDummy)

  /** Backtracks from this node to the initial node. */
  def pathToRoot: Seq[N] = self.iterate(_.parent).takeUntil(_.isDummy)

}

trait NodeWithParent[+T] extends BackwardNode[T] with NodeWithParentLike[T, NodeWithParent[T]] { self ⇒

  override def map[U](f: T ⇒ U): NodeWithParent[U] = new NodeWithParent[U] {
    def parent = self.parent.map(f)
    def data = f(self.data)
    override def isDummy = self.isDummy
  }

}
