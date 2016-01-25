package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BiTreeNodeLike[+T, +N <: BiTreeNodeLike[T, N]] extends BiNodeLike[T, N] with NodeWithParentLike[T, N] { self: N =>
  def parent: N
  override def pred = ListSeq(parent).filter(_.notDummy)
  def children: Seq[N]
  override def succ = children

}

trait BiTreeNode[+T] extends BiNode[T] with TreeNode[T] with NodeWithParent[T] with BiTreeNodeLike[T, BiTreeNode[T]] { self =>

  override def map[U](f: T => U): BiTreeNode[U] = new BiTreeNode[U] {
    def isDummy = self.isDummy
    def children = self.children.map(_.map(f))
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

}
