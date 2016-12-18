package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents a multi-way tree in which the children of each node are ordered (form a sequence instead of a set).
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait OrderedTree[+T] extends Tree[T] { self =>

  import OrderedTree._

  def rootNode: OrderedTreeNode[T]

  override def children = rootNode.children map ofRootNode

  // HELPER FUNCTIONS

  override def map[U](f: T => U): OrderedTree[U] = {
    class MappedNode(n: OrderedTreeNode[T]) extends OrderedTreeNode[U] {
      def children = n.children.map(x => new MappedNode(x))
      def data = f(n.data)
      def isDummy = n.isDummy
    }
    ofRootNode(new MappedNode(self.rootNode))
  }

  def zip[U](that: OrderedTree[U]) = (self zipWith that)((t, u) => (t, u))

  def zipWith[U, V](that: OrderedTree[U])(f: (T, U) => V): OrderedTree[V] = {
    class ZippedWithNode(t: OrderedTreeNode[T], u: OrderedTreeNode[U]) extends OrderedTreeNode[V] {
      def children = (t.children zipWith u.children) ((nt, nu) => new ZippedWithNode(nt, nu))
      def data = f(t.data, u.data)
      def isDummy = t.isDummy || u.isDummy
    }
    ofRootNode(new ZippedWithNode(self.rootNode, that.rootNode))
  }

  /**
   * '''Lazily''' performs the Knuth transform on this tree, i.e. representing this multi-way ordered tree
   * into its equivalent left-child-right-sibling (LC-RS) binary tree.
   * @example {{{
   *  ┌     a   ┐                   ┌     a   ┐
   *  │    /|\  │                   │    /    │
   *  │   b c d │                   │   b     │
   *  │  / \    │.knuthTransform == │  / \    │
   *  └ e   f   ┘                   │ e   c   │
   *                                │  \   \  │
   *                                └   f   d ┘
   * }}}
   * @return Its corresponding binary tree
   */
  def knuthTransform: BinaryTree[T] = new AbstractBinaryTree[T] {
    class LcRsNode(val node: SeqNode[OrderedTreeNode[T]]) extends BinaryTreeNode[T] {
      def isDummy = node.isDummy || node.data.isDummy
      def data = node.data.data
      def leftNode = new LcRsNode(node.data.children.headNode)
      def rightNode = new LcRsNode(node.next)
    }
    def rootNode = new LcRsNode(ListSeq(self.rootNode).headNode)
    override def inverseKnuthTransform = self
  }

  // Comonadic operation
  override def subtrees: OrderedTree[OrderedTree[T]] = {
    class TreeOfTreeNode(n: OrderedTreeNode[T]) extends OrderedTreeNode[OrderedTree[T]] {
      def children = n.children.map(tn => new TreeOfTreeNode(tn))
      def data = ofRootNode(n)
      def isDummy = n.isDummy
    }
    ofRootNode(new TreeOfTreeNode(self.rootNode))
  }

  override def toString =
    "(" + root.toString +
      (if (children.size == 0) "" else " ") +
      children.buildString(" ") +
      ")"
  //endregion


}

object OrderedTree {

  def unapply[T](xs: OrderedTree[T]) = {
    if (xs.isEmpty) None
    else Some((xs.root, xs.children))
  }

  object Empty extends OrderedTree[Nothing] {
    def rootNode = OrderedTreeNode.dummy
    def unapply[T](xs: OrderedTree[T]) = xs.isEmpty
  }

  def ofRootNode[T](n: OrderedTreeNode[T]): OrderedTree[T] = new OrderedTree[T] {
    def rootNode = n
  }

  def single[T](x: => T): OrderedTree[T] = ofRootNode {
    new OrderedTreeNode[T] {
      def children = Seq.Empty
      def data = x
      def isDummy = false
    }
  }

  def infinite[T](x: => T): OrderedTree[T] = ofRootNode {
    new OrderedTreeNode[T] {
      def children = Seq.infinite(this)
      def data = x
      def isDummy = false
    }
  }

  implicit object Comonad extends Comonad[OrderedTree] {
    def id[X](u: OrderedTree[X]): X = u.root
    def extend[X, Y](wx: OrderedTree[X])(f: OrderedTree[X] => Y): OrderedTree[Y] = wx.subtrees.map(f)
  }

  implicit object ZipIdiom extends Idiom[OrderedTree] {
    def id[X](u: X) = OrderedTree.infinite(u)
    def liftedMap[X, Y](mx: OrderedTree[X])(mf: OrderedTree[X => Y]) = (mx zipWith mf) { (x, f) => f(x) }
    override def product[X, Y](mx: OrderedTree[X])(my: OrderedTree[Y]) = mx zip my
    override def productMap[X, Y, Z](mx: OrderedTree[X], my: OrderedTree[Y])(f: (X, Y) => Z) = (mx zipWith my)(f)
  }

}

abstract class AbstractOrderedTree[T] extends OrderedTree[T]
