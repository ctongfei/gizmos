package poly.collection.node

import poly._
import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has at most two successors.
 * It is the type of nodes in a binary tree ([[collection.BinaryTree]]).
 * @since 0.1.0
 */
trait BinaryTreeNode[+T] extends Node[T] { self =>

  def left: BinaryTreeNode[T]
  def right: BinaryTreeNode[T]
  def succ: Enumerable[BinaryTreeNode[T]] = ListSeq.applyNotNull(right, left)

  /**
   * Returns a new binary tree node by applying a function to all nodes accessible from this node.
   * @param f
   * @tparam U
   * @return
   */
  override def map[U](f: T => U): BinaryTreeNode[U] = new BinaryTreeNode[U] {
    def left = self.left.map(f)
    def right = self.right.map(f)
    def data = f(self.data)
  }

  def zip[U](that: BinaryTreeNode[U]): BinaryTreeNode[(T, U)] = new BinaryTreeNode[(T, U)] {
    def left = self.left zip that.left
    def right = self.right zip that.right
    def data = (self.data, that.data)
  }

  /**
   * Performs pre-order traversal from this node.
   * @return A non-strict sequence of the pre-order traversal.
   */
  def preOrder: Enumerable[T] = Enumerable.ofEnumerator {
    new Enumerator[T] {
      private val s = ArrayStack[BinaryTreeNode[T]](self)
      private var curr: BinaryTreeNode[T] = null
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.pop()
        if (curr.right != null) s.push(curr.right)
        if (curr.left != null) s.push(curr.left)
        true
      }
      def current: T = curr.data
    }
  }

  /**
   * Performs in-order traversal from this node.
   * @return A non-strict sequence of the in-order traversal.
   */
  def inOrder: Enumerable[T] = Enumerable.ofEnumerator {
    new Enumerator[T] {
      private[this] val s = ArrayStack[BinaryTreeNode[T]]()
      private[this] var v: BinaryTreeNode[T] = null
      private[this] var curr: BinaryTreeNode[T] = null
      pushLeft(self)

      private[this] def pushLeft(n: BinaryTreeNode[T]) = {
        var node = n
        while (node != null) {
          s.push(node)
          node = node.left
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.pop()
        curr = v
        v = v.right
        if (v != null) pushLeft(v)
        true
      }
      def current: T = curr.data
    }
  }

  /**
   * Performs post-order traversal from this node.
   * @return A non-strict sequence of the post-order traversal.
   */
  def postOrder: Enumerable[T] = Enumerable.ofEnumerator {
    new Enumerator[T] {
      private[this] val s = ArrayStack[BinaryTreeNode[T]]()
      private[this] var v: BinaryTreeNode[T] = null
      private[this] var curr: BinaryTreeNode[T] = null
      pushLeft(self)

      private[this] def pushLeft(n: BinaryTreeNode[T]) = {
        var node = n
        while (node ne null) {
          s.push(node)
          node = node.left
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.pop()
        curr = v
        if (s.notEmpty && (s.top.left == v)) {
          v = s.top.right
          if (v ne null) pushLeft(v)
        }
        true
      }
      def current: T = curr.data
    }
  }

}

trait BiBinaryTreeNode[+T] extends BiNode[T] with BinaryTreeNode[T] with SinglePredNode[T] { self =>

  def left: BiBinaryTreeNode[T]
  def right: BiBinaryTreeNode[T]
  def parent: BiBinaryTreeNode[T]
  override def pred: Enumerable[BiBinaryTreeNode[T]] = ListSeq.applyNotNull(parent)
  override def succ: Enumerable[BiBinaryTreeNode[T]] = ListSeq.applyNotNull(right, left)

  override def map[U](f: T => U): BiBinaryTreeNode[U] = new BiBinaryTreeNode[U] {
    def left = self.left.map(f)
    def right = self.right.map(f)
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

}
