package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has at most two successors.
 * @since 0.1.0
 */
trait BinaryTreeNode[+T] extends Node[T] { self =>

  def left: BinaryTreeNode[T]
  def right: BinaryTreeNode[T]
  def succ: Enumerable[BinaryTreeNode[T]] = ListSeq(right, left) //TODO: null?

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

trait BidiBinaryTreeNode[+T] extends
  BidiNode[T] with BinaryTreeNode[T] with SinglePredNode[T]
{ self =>

  def left: BidiBinaryTreeNode[T]
  def right: BidiBinaryTreeNode[T]
  def parent: BidiBinaryTreeNode[T]
  override def pred: Enumerable[BidiBinaryTreeNode[T]] = ListSeq(parent)
  override def succ: Enumerable[BidiBinaryTreeNode[T]] = ListSeq(right, left)

  override def map[U](f: T => U): BidiBinaryTreeNode[U] = new BidiBinaryTreeNode[U] {
    def left = self.left.map(f)
    def right = self.right.map(f)
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

}
