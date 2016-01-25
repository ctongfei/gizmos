package poly.collection.node

import poly._
import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has at most two successors.
 * It is the type of nodes in a binary tree ([[poly.collection.BinaryTree]]).
 * @since 0.1.0
 */
trait BinaryTreeNode[+T] extends BinaryTreeNodeLike[T, BinaryTreeNode[T]] { self =>
  def data: T
  def left: BinaryTreeNode[T]
  def right: BinaryTreeNode[T]

  /**
   * Returns a new binary tree node by applying a function to all nodes accessible from this node.
   * @param f
   * @tparam U
   * @return
   */
  def map[U](f: T => U): BinaryTreeNode[U] = new BinaryTreeNode[U] {
    def left = self.left.map(f)
    def right = self.right.map(f)
    def data = f(self.data)
    override def isDummy = self.isDummy
  }

  def zip[U](that: BinaryTreeNode[U]): BinaryTreeNode[(T, U)] = new BinaryTreeNode[(T, U)] {
    def left = self.left zip that.left
    def right = self.right zip that.right
    def data = (self.data, that.data)
    override def isDummy = self.isDummy || that.isDummy
  }

}

object BinaryTreeNode {

  object Dummy extends BinaryTreeNode[Nothing] {
    def data = throw new NoSuchElementException
    def left = Dummy
    def right = Dummy
    def isDummy = true
  }

  def unapply[T](t: BinaryTreeNode[T]): Option[(T, BinaryTreeNode[T], BinaryTreeNode[T])] = {
    if (t.isDummy) None else Some((t.data, t.left, t.right))
  }

}

