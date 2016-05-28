package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BiBinaryTreeNodeLike[+T, +N <: BiBinaryTreeNodeLike[T, N]]
  extends BinaryTreeNodeLike[T, N] with NodeWithParentLike[T, N]
{ self: N ⇒

  def left: N
  def right: N
  def parent: N

  override def pred: Iterable[N] = ListSeq(parent).filter(_.notDummy)

}

/**
 * Represents a node that has at most two successors and at most one predecessor.
 * It is the type of nodes in a binary tree with a parent pointer.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiBinaryTreeNode[+T] extends BiNode[T] with BinaryTreeNode[T] with BiBinaryTreeNodeLike[T, BiBinaryTreeNode[T]] { self ⇒

  override def map[U](f: T ⇒ U): BiBinaryTreeNode[U] = new BiBinaryTreeNode[U] {
    def left = self.left map f
    def right = self.right map f
    def parent = self.parent map f
    def data = f(self.data)
    override def isDummy = self.isDummy
  }

}
