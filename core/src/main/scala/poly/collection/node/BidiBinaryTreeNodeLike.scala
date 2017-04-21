package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BidiBinaryTreeNodeLike[+T, +N <: BidiBinaryTreeNodeLike[T, N]]
  extends BinaryTreeNodeLike[T, N] with NodeWithParentLike[T, N] with BidiNodeLike[T, N]
{ self: N =>

  def leftNode: N
  def rightNode: N
  def parent: N

  override def pred: Iterable[N] = ListSeq(parent).filter(_.notDummy)

}

/**
 * Represents a node that has at most two successors and at most one predecessor.
 * It is the type of nodes in a binary tree with a parent pointer.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiBinaryTreeNode[+T] extends BidiNode[T] with BinaryTreeNode[T] with BidiBinaryTreeNodeLike[T, BidiBinaryTreeNode[T]]
