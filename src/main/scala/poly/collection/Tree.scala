package poly.collection

import poly.algebra.hkt._
import poly.collection.conversion.FromScala._
import poly.collection.node._

/**
 * Represents a multi-way tree in which the children of a node are not ordered.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Tree[+T] { self =>

  import Tree._

  def rootNode: TreeNode[T]

  def root = rootNode.data

  def children = rootNode.children map ofRootNode

  def isEmpty = rootNode.isDummy

  def map[U](f: T => U) = {
    class MappedNode(n: TreeNode[T]) extends TreeNode[U] {
      def children = n.children.map(x => new MappedNode(x))
      def data = f(n.data)
      def isDummy = n.isDummy
    }
    ofRootNode(new MappedNode(self.rootNode))
  }

  def fold[U](f: (T, Iterable[U]) => U): U =
    f(root, children.map(_ fold f)) //TODO: makes it non-recursive

  /**
   * The comonadic operation on trees.
   */
  def subtrees: Tree[Tree[T]] = {
    class TreeOfTreeNode(n: TreeNode[T]) extends TreeNode[Tree[T]] {
      def children = n.children.map(tn => new TreeOfTreeNode(tn))
      def data = ofRootNode(n)
      def isDummy = n.isDummy
    }
    ofRootNode(new TreeOfTreeNode(self.rootNode))
  }

  /**
   * '''Lazily''' traverses this tree in pre-order (depth-first order).
   * @example {{{
   *    ┌      a    ┐
   *    │     /|\   │
   *    │    b c d  │
   *    │   / \     │.preOrder == (a, b, e, f, c, d)
   *    └  e   f    ┘
   * }}}
   */
  def preOrder = rootNode.preOrder map { _.data }

  /**
   * '''Lazily''' traverses this tree in level order (breadth-first order).
   * @example {{{
   *    ┌      a    ┐
   *    │     /|\   │
   *    │    b c d  │
   *    │   / \     │.levelOrder == (a, b, c, d, e, f)
   *    └  e   f    ┘
   * }}}
   */
  def levelOrder = rootNode.levelOrder map { _.data }

  /**
   * '''Lazily''' traverses this tree in post-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     /|\   │
   *    │    b c d  │
   *    │   / \     │.postOrder == (e, f, b, c, d, a)
   *    └  e   f    ┘
   * }}}
   */ // == fold { (n, cs) => cs.flatten :+ n }
  def postOrder = rootNode.postOrder.map { _.data }

  def leaves = rootNode.preOrder.filter(_.isLeaf).map(_.data)

  override def toString = self match {
    case Empty       => ""
    case Tree(r, cs) => cs match {
      case Iterable.Empty() => s"$r"
      case _                => s"($r ${cs.map(_.toString).buildString(" ")})"
    }
  }
}

object Tree {

  def apply[T](r: T, c: Tree[T]*): Tree[T] = new Tree[T] {
    def rootNode = new TreeNode[T] {
      def children = c.map(_.rootNode)
      def data = r
      def isDummy = false
    }
  }

  def unapply[T](xs: Tree[T]) = {
    if (xs.isEmpty) None
    else Some(xs.root, xs.children)
  }

  object Empty extends Tree[Nothing] {
    def rootNode = TreeNode.Dummy
    def unapply[T](t: Tree[T]) = t.isEmpty
  }

  def ofRootNode[T](n: TreeNode[T]): Tree[T] = new Tree[T] {
    def rootNode = n
  }

  def single[T](x: => T): Tree[T] = ofRootNode {
    new TreeNode[T] {
      def children = Iterable.Empty
      def data = x
      def isDummy = false
    }
  }

  def infinite[T](x: => T): Tree[T] = ofRootNode {
    new TreeNode[T] {
      def children = Iterable.infinite(this)
      def data = x
      def isDummy = false
    }
  }

  implicit object Comonad extends Comonad[Tree] {
    def id[X](u: Tree[X]) = u.root
    def extend[X, Y](wx: Tree[X])(f: Tree[X] => Y) = wx.subtrees map f
  }

}

private[poly] object TreeT {


}