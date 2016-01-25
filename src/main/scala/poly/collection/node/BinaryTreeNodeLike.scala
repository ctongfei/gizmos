package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BinaryTreeNodeLike[+T, +N <: BinaryTreeNodeLike[T, N]] extends ForwardNodeLike[T, N] { self: N =>

  def left: N
  def right: N

  def children = ListSeq(left, right).filter(_.notDummy)

  override def succ: Iterable[N] = children

  /**
   * Performs pre-order traversal from this node.
   * @return A non-strict sequence of the pre-order traversal.
   */
  def preOrder: Iterable[N] = Iterable.ofIterator {
    new Iterator[N] {
      private val s = ArrayStack[N](self)
      private var curr: N = default[N]
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.pop()
        if (curr.right.notDummy) s.push(curr.right)
        if (curr.left.notDummy) s.push(curr.left)
        true
      }
      def current = curr
    }
  }


  /**
   * Performs in-order traversal from this node. $LAZY
   * @return A non-strict sequence of the in-order traversal.
   */
  def inOrder: Iterable[N] = Iterable.ofIterator {
    new Iterator[N] {
      private[this] val s = ArrayStack[N]()
      private[this] var v: N = default[N]
      private[this] var curr: N = default[N]
      pushLeft(self)

      private[this] def pushLeft(n: N) = {
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
      def current = curr
    }
  }

  /**
   * Performs post-order traversal from this node. $LAZY
   * @return A non-strict sequence of the post-order traversal.
   */
  def postOrder: Iterable[N] = Iterable.ofIterator {
    new Iterator[N] {
      private[this] val s = ArrayStack[N]()
      private[this] var v: N = default[N]
      private[this] var curr: N = default[N]
      pushLeft(self)

      private[this] def pushLeft(n: N) = {
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
      def current = curr
    }
  }

}

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

