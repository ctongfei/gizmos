package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BinaryTreeNodeLike[+T, +N <: BinaryTreeNodeLike[T, N]] extends ForwardNodeLike[T, N] { self: N =>

  def leftNode: N
  def rightNode: N

  def children = ListSeq(leftNode, rightNode).filter(_.notDummy)

  override def succ: Iterable[N] = children

  def isLeaf = leftNode.isDummy && rightNode.isDummy

  def notLeaf = !isLeaf

  /**
   * Performs pre-order traversal from this node.
   * @return A non-strict sequence of the pre-order traversal.
   */
  def preOrderTraversal: Iterable[N] = Iterable.ofIterator {
    new AbstractIterator[N] {
      private val s = ArrayStack[N](self)
      private var curr: N = default[N]
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.dequeue()
        if (curr.rightNode.notDummy) s.enqueue(curr.rightNode)
        if (curr.leftNode.notDummy) s.enqueue(curr.leftNode)
        true
      }
      def current = curr
    }
  }

  /**
   * Performs in-order traversal from this node. $LAZY
   * @return A non-strict sequence of the in-order traversal.
   */
  def inOrderTraversal: BidiIterable[N] = {
    class ForwardInOrderIterator extends Iterator[N] {
      private[this] val s = ArrayStack[N]()
      private[this] var v: N = default[N]
      private[this] var curr: N = default[N]
      pushLeft(self)

      private[this] def pushLeft(n: N) = {
        var node = n
        while (node.notDummy) {
          s.enqueue(node)
          node = node.leftNode
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.dequeue()
        curr = v
        v = v.rightNode
        if (v.notDummy) pushLeft(v)
        true
      }
      def current = curr
    }
    class BackwardInOrderIterator extends Iterator[N] {
      private[this] val s = ArrayStack[N]()
      private[this] var v: N = default[N]
      private[this] var curr: N = default[N]
      pushRight(self)

      private[this] def pushRight(n: N) = {
        var node = n
        while (node.notDummy) {
          s.enqueue(node)
          node = node.rightNode
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.dequeue()
        curr = v
        v = v.leftNode
        if (v.notDummy) pushRight(v)
        true
      }
      def current = curr
    }
    BidiIterable.ofIterator(new ForwardInOrderIterator, new BackwardInOrderIterator)
  }

  /**
   * Performs post-order traversal from this node. $LAZY
   * @return A non-strict sequence of the post-order traversal.
   */
  def postOrderTraversal: Iterable[N] = Iterable.ofIterator {
    new AbstractIterator[N] {
      private[this] val s = ArrayStack[N]()
      private[this] var v: N = default[N]
      private[this] var curr: N = default[N]
      pushLeft(self)

      private[this] def pushLeft(n: N) = {
        var node = n
        while (node.notDummy) {
          s.enqueue(node)
          node = node.leftNode
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.dequeue()
        curr = v
        if (s.notEmpty && (s.front.leftNode == v)) {
          v = s.front.rightNode
          if (v.notDummy) pushLeft(v)
        }
        true
      }
      def current = curr
    }
  }

}

trait BinaryTreeNode[+T] extends BinaryTreeNodeLike[T, BinaryTreeNode[T]]

object BinaryTreeNode {

  object Dummy extends BinaryTreeNode[Nothing] {
    def data = throw new NoSuchElementException
    def leftNode = Dummy
    def rightNode = Dummy
    def isDummy = true
  }

  def unapply[T](t: BinaryTreeNode[T]): Option[(T, BinaryTreeNode[T], BinaryTreeNode[T])] = {
    if (t.isDummy) None else Some((t.data, t.leftNode, t.rightNode))
  }

}

