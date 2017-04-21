package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BidiOrderedTreeNodeLike[+T, +N <: BidiOrderedTreeNodeLike[T, N]] extends BidiTreeNodeLike[T, N] with NodeWithParentLike[T, N] { self: N =>
  def parent: N
  def children: Seq[N]
  override def succ = children

}

trait BidiOrderedTreeNode[+T] extends BidiTreeNode[T] with OrderedTreeNode[T] with NodeWithParent[T] with BidiOrderedTreeNodeLike[T, BidiOrderedTreeNode[T]]
