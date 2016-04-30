package poly.collection

import poly.algebra.hkt._
import poly.collection.exception._
import poly.collection.impl._
import poly.collection.mut._
import poly.collection.node._
import poly.collection.ops._

/**
 * Represents a binary tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BinaryTree[+T] { self =>

  import BinaryTree._

  def dummy: BinaryTreeNode[T] = new BinaryTreeNode[T] {
    def data: T = throw new DummyNodeException
    def left: BinaryTreeNode[T] = rootNode
    def right: BinaryTreeNode[T] = rootNode
    def isDummy: Boolean = true
  }

  def rootNode: BinaryTreeNode[T]

  def root: T = rootNode.data

  /** Returns the maximal height of this tree. */ //TODO: a recursive-free version?
  def height: Int = foldBottomUp(0)((l, r, _) => math.max(l, r) + 1)

  /** Returns the number of nodes in this tree. */
  def size: Int = rootNode.preOrder.size

  def isEmpty = rootNode.isDummy

  /**
   * Returns the ''i''th node of this binary tree.
   * The ordinal is defined as follows:
   *  <ul>
   *    <li> The index of the root node is 0; </li>
   *    <li> The left child of node ''i'' is 2''i'' + 1; </li>
   *    <li> The right child of node ''i'' is 2''i'' + 2. </li>
   * </ul>
   * @param i Index
   * @return The ''i''th node with the index defined above
   */
  def apply(i: Int): T = { //TODO: currently wrong
    var x = i + 1
    var curr = rootNode
    var depth = 1
    while (depth <= x) {
      x / depth match {
        case 0 => curr = curr.left
        case 1 => curr = curr.right
      }
      if (curr.isDummy) throw new KeyNotFoundException(i)
      x %= depth
      depth <<= 1
    }
    curr.data
  }

  def isDefinedAt(i: Int): Boolean = ???

  // HELPER FUNCTIONS

  /**
   * Returns the left subtree of this binary tree. $LAZY $CX_1
   * @return The left subtree
   */
  def left: BinaryTree[T] = ofRootNode(rootNode.left)

  /**
   * Returns the right subtree of this binary tree. $LAZY $CX_1
   * @return The right subtree
   */
  def right: BinaryTree[T] = ofRootNode(rootNode.right)

  def map[U](f: T => U): BinaryTree[U] = ofRootNode(rootNode.map(f))

  /** Folds a binary tree bottom-up. This is analogous to the sequence `foldRight` in that both
    * are catamorphisms on recursive structures.
    */ //TODO: non-recursive version?
  def foldBottomUp[U](z: U)(f: (U, U, T) => U): U = self match {
    case BinaryTree.empty() => z
    case (l :/ n \: r) => f(l.foldBottomUp(z)(f), r.foldBottomUp(z)(f), n)
  }

  def fold[U >: T](z: U)(f: (U, U, U) => U) = foldBottomUp(z)(f)

  /**
   * Zips two binary trees into one. $LAZY
   * @example {{{
   *   ┌      a    ┐     ┌    1      ┐    ┌               ┐
   *   │     / \   │     │   / \     │    │     (a,1)     │
   *   │    b   c  │ zip │  2   3    │ == │     /   \     │
   *   │   / \     │     │     / \   │    │ (b,2)   (c,3) │
   *   └  d   e    ┘     └    4   5  ┘    └               ┘
   * }}}
   */
  def zip[U](that: BinaryTree[U]) = ofRootNode(self.rootNode zip that.rootNode)

  /**
   * '''Lazily''' traverses this binary tree in pre-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.preOrder == (a, b, d, e, c)
   *    └  d   e    ┘
   * }}}
   */
  def preOrder = rootNode.preOrder.map(_.data)

  /**
   * '''Lazily''' traverses this binary tree in in-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.inOrder == (d, b, e, a, c)
   *    └  d   e    ┘
   * }}}
   */
  def inOrder = rootNode.inOrder.map(_.data)

  /**
   * Returns the reflected mirror image of this binary tree.
   * @example {{{
   *  ┌      a    ┐            ┌    a      ┐
   *  │     / \   │            │   / \     │
   *  │    b   c  │            │  c   b    │
   *  │   / \     │.reflect == │     / \   │
   *  └  d   e    ┘            └    e   d  ┘
   * }}}
   */
  def reflect: BinaryTree[T] = new AbstractBinaryTree[T] {
    def rootNode = self.rootNode.reflect
    override def reflect = self
  }

  /**
   * '''Lazily''' traverses this binary tree in post-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.postOrder = (d, e, b, c, a)
   *    └  d   e    ┘
   * }}}
   */
  def postOrder = rootNode.postOrder.map(_.data)

  /**
   * '''Lazily''' traverses this binary tree in level-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.levelOrder == (a, b, c, d, e)
   *    └  d   e    ┘
   * }}}
   */
  def levelOrder = rootNode.breadthFirstTreeTraversal.map(_.data)

  def leaves = rootNode.preOrder.filter(_.isLeaf).map(_.data)

  /**
   * Performs inverse Knuth transform on this binary tree, i.e., recover the multi-way tree
   * compactly represented by this binary tree through Knuth transform (left-child-right-sibling
   * representation). $LAZY
   * @example {{{
   *  ┌     a   ┐
   *  │    /    │                          ┌     a   ┐
   *  │   b     │                          │    /|\  │
   *  │  / \    │.inverseKnuthTransform == │   b c d │
   *  │ e   c   │                          │  / \    │
   *  │  \   \  │                          └ e   f   ┘
   *  └   f   d ┘
   * }}}
   */
  def inverseKnuthTransform: Tree[T] = new Tree[T] {
    class InverseKnuthTransformedTreeNode(val node: BinaryTreeNode[T]) extends TreeNode[T] {
      override def isDummy = node.isDummy
      def children = node.left.iterate(_.right).takeUntil(_.isDummy).map(btn => new InverseKnuthTransformedTreeNode(btn))
      def data = node.data
    }
    def rootNode = new InverseKnuthTransformedTreeNode(self.rootNode)
    override def knuthTransform = self
  }

  def subtrees: BinaryTree[BinaryTree[T]] = {
    class SubtreeNode(n: BinaryTreeNode[T]) extends BinaryTreeNode[BinaryTree[T]] {
      def data = ofRootNode(n)
      def left = new SubtreeNode(n.left)
      def right = new SubtreeNode(n.right)
      def isDummy = n.isDummy
    }
    ofRootNode(new SubtreeNode(self.rootNode))
  }

  ///**
  // * Constructs a binary tree using two sub-binary-trees and a root element.
  // * @example {{{ left :/ root \: right }}}
  // */
  //def :/[U >: T](n: U) = new {
  //  def :\(r: BinaryTree[U]): BinaryTree[U] = new AbstractBinaryTree[U] {
  //    def rootNode = new BinaryTreeNode[U] {
  //      def data = n
  //      def left = self.rootNode
  //      def right = r.rootNode
  //      def isDummy = false
  //    }
  //  }
  //}


  def asTree: Tree[T] = {
    class BinaryTreeAsTreeNode(n: BinaryTreeNode[T]) extends TreeNode[T] {
      def children = ListSeq(n.left, n.right).filter(_.notDummy).map(n => new BinaryTreeAsTreeNode(n))
      def data = n.data
      def isDummy = n.isDummy
    }
    Tree.ofRootNode(new BinaryTreeAsTreeNode(self.rootNode))
  }

  override def toString = if (isEmpty) "#" else s"($root $left $right)"

}

object BinaryTree {

  @inline def parentIndex(i: Int) = (i - 1) / 2
  @inline def leftChildIndex(i: Int) = 2 * i + 1
  @inline def rightChildIndex(i: Int) = 2 * i + 2

  def unapply[T](bt: BinaryTree[T]) = {
    if (bt.isEmpty) None
    else Some((bt.root, bt.left, bt.right))
  }

  def infinite[T](x: => T): BinaryTree[T] = ofRootNode {
    new BinaryTreeNode[T] {
      def data = x
      def left = this
      def right = this
      def isDummy = false
    }
  }

  object empty extends BinaryTree[Nothing] {
    def rootNode = BinaryTreeNode.Dummy
    def unapply[T](bt: BinaryTree[T]) = bt.isEmpty
  }

  def ofRootNode[T](n: BinaryTreeNode[T]): BinaryTree[T] = new BinaryTree[T] {
    override def rootNode = n
  }

  implicit object ZipIdiom extends Idiom[BinaryTree] {
    def id[X](u: X): BinaryTree[X] = infinite(u)
    def liftedMap[X, Y](mx: BinaryTree[X])(mf: BinaryTree[(X) => Y]): BinaryTree[Y] = (mx zip mf).map { case (x, f) => f(x) }
    override def map[T, U](t: BinaryTree[T])(f: T => U): BinaryTree[U] = t map f
  }

  implicit object Comonad extends Comonad[BinaryTree] {
    def id[X](u: BinaryTree[X]) = u.root
    def extend[X, Y](wx: BinaryTree[X])(f: BinaryTree[X] => Y) = wx.subtrees map f
  }

}

abstract class AbstractBinaryTree[+T] extends BinaryTree[T]
