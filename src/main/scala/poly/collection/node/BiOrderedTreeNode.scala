package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BiOrderedTreeNodeLike[+T, +N <: BiOrderedTreeNodeLike[T, N]] extends BiNodeLike[T, N] with NodeWithParentLike[T, N] { self: N =>
  def parent: N
  override def pred = ListSeq(parent).filter(_.notDummy)
  def children: Seq[N]
  override def succ = children

}

trait BiOrderedTreeNode[+T] extends BiNode[T] with OrderedTreeNode[T] with NodeWithParent[T] with BiOrderedTreeNodeLike[T, BiOrderedTreeNode[T]] { self =>

  override def map[U](f: T => U): BiOrderedTreeNode[U] = new BiOrderedTreeNode[U] {
    def isDummy = self.isDummy
    def children = self.children.map(_.map(f))
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

}
