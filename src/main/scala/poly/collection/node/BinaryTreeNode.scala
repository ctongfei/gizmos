package poly.collection.node

import poly._
import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has at most two successors.
 * It is the type of nodes in a binary tree ([[poly.collection.BinaryTree]]).
 * @since 0.1.0
 */
trait BinaryTreeNode[+T] extends TreeNode[T] { self =>
  def data: T
  def left: BinaryTreeNode[T]
  def right: BinaryTreeNode[T]
  def children = ListSeq(left, right).filter(_.notDummy)
  override def succ: Iterable[BinaryTreeNode[T]] = ListSeq(right, left).filter(_.notDummy)


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
    override def isDummy = self.isDummy
  }

  def zip[U](that: BinaryTreeNode[U]): BinaryTreeNode[(T, U)] = new BinaryTreeNode[(T, U)] {
    def left = self.left zip that.left
    def right = self.right zip that.right
    def data = (self.data, that.data)
    override def isDummy = self.isDummy || that.isDummy
  }

  /**
   * Performs pre-order traversal from this node.
   * @return A non-strict sequence of the pre-order traversal.
   */
  def preOrder: Iterable[T] = Iterable.ofIterator {
    new Iterator[T] {
      private val s = ArrayStack[BinaryTreeNode[T]](self)
      private var curr: BinaryTreeNode[T] = BinaryTreeNode.Dummy
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.pop()
        if (curr.right.notDummy) s.push(curr.right)
        if (curr.left.notDummy) s.push(curr.left)
        true
      }
      def current: T = curr.data
    }
  }

  /**
   * Performs in-order traversal from this node.
   * @return A non-strict sequence of the in-order traversal.
   */
  def inOrder: Iterable[T] = Iterable.ofIterator {
    new Iterator[T] {
      private[this] val s = ArrayStack[BinaryTreeNode[T]]()
      private[this] var v: BinaryTreeNode[T] = BinaryTreeNode.Dummy
      private[this] var curr: BinaryTreeNode[T] = BinaryTreeNode.Dummy
      pushLeft(self)

      private[this] def pushLeft(n: BinaryTreeNode[T]) = {
        var node = n
        while (node.notDummy) {
          s.push(node)
          node = node.left
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.pop()
        curr = v
        v = v.right
        if (v.notDummy) pushLeft(v)
        true
      }
      def current: T = curr.data
    }
  }

  /**
   * Performs post-order traversal from this node.
   * @return A non-strict sequence of the post-order traversal.
   */
  def postOrder: Iterable[T] = Iterable.ofIterator {
    new Iterator[T] {
      private[this] val s = ArrayStack[BinaryTreeNode[T]]()
      private[this] var v: BinaryTreeNode[T] = BinaryTreeNode.Dummy
      private[this] var curr: BinaryTreeNode[T] = BinaryTreeNode.Dummy
      pushLeft(self)

      private[this] def pushLeft(n: BinaryTreeNode[T]) = {
        var node = n
        while (node.notDummy) {
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
          if (v.notDummy) pushLeft(v)
        }
        true
      }
      def current: T = curr.data
    }
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
