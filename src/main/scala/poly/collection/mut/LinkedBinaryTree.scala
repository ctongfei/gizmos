package poly.collection.mut

import poly.collection._
import poly.collection.impl.BinaryTreeNode

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedBinaryTree[T] {

  private[this] val dummy = new BinaryTreeNode[T](default[T])
  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def preOrder: Enumerable[BinaryTreeNode[T]] = new Enumerable[BinaryTreeNode[T]] {
    def enumerator: Enumerator[BinaryTreeNode[T]] = new Enumerator[BinaryTreeNode[T]] {
      def advance(): Boolean = ???

      def current: BinaryTreeNode[T] = ???
    }
  }



}
