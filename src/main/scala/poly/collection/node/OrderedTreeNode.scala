package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait OrderedTreeNodeLike[+T, +N <: OrderedTreeNodeLike[T, N]] extends TreeNodeLike[T, N] { self: N =>

  def children: Seq[N]

}

trait OrderedTreeNode[+T] extends TreeNode[T] with OrderedTreeNodeLike[T, OrderedTreeNode[T]] { self =>

  def children: Seq[OrderedTreeNode[T]]

  override def map[U](f: T => U): OrderedTreeNode[U] = new OrderedTreeNode[U] {
    def children = self.children.map(_ map f)
    def data = f(self.data)
    def isDummy = self.isDummy
  }

}

object OrderedTreeNode {

  object dummy extends OrderedTreeNode[Nothing] {
    def children = Seq.Empty
    def data = throw new NoSuchElementException
    def isDummy = true
  }

}