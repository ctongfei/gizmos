package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents a multi-way tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Tree[+T] { self ⇒

  import Tree._

  def rootNode: TreeNode[T]

  def root = rootNode.data

  def children = rootNode.children map ofRootNode

  // HELPER FUNCTIONS

  /**
   * Folds a tree bottom-up using the specific function.
   */
  //TODO: a non-recursive faster implementation?
  def foldBottomUp[U](z: U)(f: (T, Seq[U]) ⇒ U): U =
    f(self.root, self.children.map(_.foldBottomUp(z)(f)))

  /**
   * '''Lazily''' performs the Knuth transform on this tree, i.e. representing this multi-way tree
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
    class LcRsNode(val node: SeqNode[TreeNode[T]]) extends BinaryTreeNode[T] {
      override def isDummy = node.isDummy || node.data.isDummy
      def data = node.data.data
      def left = new LcRsNode(node.data.children.headNode)
      def right = new LcRsNode(node.next)
    }
    def rootNode = new LcRsNode(ListSeq(self.rootNode).headNode)
    override def inverseKnuthTransform = self
  }

  // Comonadic operation
  def subtrees: Tree[Tree[T]] = {
    class TreeOfTreeNode(n: TreeNode[T]) extends TreeNode[Tree[T]] {
      def children: Seq[TreeNode[Tree[T]]] = n.children.map(tn ⇒ new TreeOfTreeNode(tn))
      def data: Tree[T] = ofRootNode(n)
      def isDummy: Boolean = n.isDummy
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
   */
  def postOrder = knuthTransform.inOrder //TODO: a more efficient implementation?

  def leaves = rootNode.preOrder.filter(_.isLeaf).map(_.data)

  override def toString =
    "(" + root.toString +
      (if (children.size == 0) "" else " ") +
      children.buildString(" ") +
      ")"
  //endregion


}

object Tree {

  def ofRootNode[T](n: TreeNode[T]): Tree[T] = new Tree[T] {
    def rootNode = n
  }

  def KnuthTransform[T]: Bijection[Tree[T], BinaryTree[T]] = new Bijection[Tree[T], BinaryTree[T]] {
    def apply(x: Tree[T]) = x.knuthTransform
    def invert(y: BinaryTree[T]) = y.inverseKnuthTransform
  }

  implicit object Comonad extends Comonad[Tree] {
    def id[X](u: Tree[X]): X = u.root
    def extend[X, Y](wx: Tree[X])(f: Tree[X] ⇒ Y): Tree[Y] = ???
  }

}

abstract class AbstractTree[T] extends Tree[T]
