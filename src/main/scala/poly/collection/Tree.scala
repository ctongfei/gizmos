package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.mut._
import poly.util.typeclass._
import poly.util.typeclass.ops._
import poly.collection.node._

/**
 * Represents a multi-way tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Tree[+T] { self =>

  import Tree._

  def rootNode: TreeNode[T]

  def root = rootNode.data

  def children = rootNode.children.map(t => ofNode(t))

  //region HELPER FUNCTIONS

  /**
   * Performs the Knuth transform on this tree, i.e. representing this multi-way tree
   * into its equivalent left-child-right-sibling binary tree. $LAZY $CX_1
   *
   * {{{
   *         a               a
   *        /|\             /
   *       b c d   ->      b
   *      / \             / \
   *     e   f           e   c
   *                      \   \
   *                       f   d
   *
   * }}}
   * @return Its corresponding binary tree
   */
  def knuthTransform: BinaryTree[T] = new AbstractBinaryTree[T] {
    class LeftChildRightSiblingBinaryTreeNode(val node: SeqNode[TreeNode[T]]) extends BinaryTreeNode[T] {
      override def isDummy = node.isDummy || node.data.isDummy
      def data = node.data.data
      def left = new LeftChildRightSiblingBinaryTreeNode(node.data.children.dummy.next)
      def right = new LeftChildRightSiblingBinaryTreeNode(node.next)
    }
    def rootNode = new LeftChildRightSiblingBinaryTreeNode(ListSeq(self.rootNode).dummy.next)
    override def inverseKnuthTransform = self
  }

  def subtrees: Tree[Tree[T]] = ???

  def preOrder = rootNode.depthFirstTreeTraversal.map(_.data)

  def levelOrder = rootNode.breadthFirstTreeTraversal.map(_.data)

  //endregion
}

object Tree {

  def ofNode[T](n: TreeNode[T]): Tree[T] = new Tree[T] {
    def rootNode = n
  }

  implicit def KnuthTransform[T]: Bijection[Tree[T], BinaryTree[T]] = new Bijection[Tree[T], BinaryTree[T]] {
    def apply(x: Tree[T]) = x.knuthTransform
    def invert(y: BinaryTree[T]) = y.inverseKnuthTransform
  }

  implicit object Comonad extends Comonad[Tree] {
    def id[X](u: Tree[X]): X = u.root
    def extend[X, Y](wx: Tree[X])(f: Tree[X] => Y): Tree[Y] = ???
  }

  /**
   * Formats a tree into an S-expression.
   * @return An S-expression that represents the specific tree.
   */
  implicit def Formatter[T: Formatter]: Formatter[Tree[T]] = new Formatter[Tree[T]] {
    def str(x: Tree[T]): String =
      "(" + x.root.str +
        (if (x.children.size == 0) "" else " ") +
        x.children.buildString(" ")(this) +
      ")"
  }
}
