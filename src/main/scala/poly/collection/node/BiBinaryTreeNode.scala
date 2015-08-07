package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has at most two successors and at most one predecessor.
 * It is the type of nodes in a binary tree with a parent pointer.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait BiBinaryTreeNode[+T] extends BiNode[T] with BinaryTreeNode[T] with SinglePredNode[T] { self =>

  def left: BiBinaryTreeNode[T]
  def right: BiBinaryTreeNode[T]
  def parent: BiBinaryTreeNode[T]
  override def pred: Enumerable[BiBinaryTreeNode[T]] = ListSeq(parent).filter(_.notDummy)
  override def succ: Enumerable[BiBinaryTreeNode[T]] = ListSeq(right, left).filter(_.notDummy)

  override def map[U](f: T => U): BiBinaryTreeNode[U] = new BiBinaryTreeNode[U] {
    def left = self.left map f
    def right = self.right map f
    def parent = self.parent map f
    def data = f(self.data)
  }

}
