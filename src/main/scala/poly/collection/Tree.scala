package poly.collection

import poly.algebra._
import poly.collection.mut._
import poly.collection.search._
import poly.util.typeclass._
import poly.util.typeclass.ops._
import poly.collection.node._

/**
 * Represents a multi-way tree.
 * @author Tongfei Chen (ctongfei@gmail.com).
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
      def left = new LeftChildRightSiblingBinaryTreeNode(node.data.children.headNode)
      def right = new LeftChildRightSiblingBinaryTreeNode(node.next)
    }
    def rootNode = new LeftChildRightSiblingBinaryTreeNode(ListSeq(self.rootNode).headNode)
    override def inverseKnuthTransform = self
  }

  def preOrder: Iterable[T] = ??? // DFS

  def levelOrder: Iterable[T] = ??? // BFS

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
