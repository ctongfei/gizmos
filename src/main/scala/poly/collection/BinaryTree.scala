package poly.collection

import poly.algebra.hkt._
import poly.collection.exception._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents a binary tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BinaryTree[+T] { self =>

  import BinaryTree._

  def dummy: BinaryTreeNode[T] = new BinaryTreeNode[T] {
    def data: T = throw new DummyNodeException
    def leftNode: BinaryTreeNode[T] = rootNode
    def rightNode: BinaryTreeNode[T] = rootNode
    def isDummy: Boolean = true
  }

  def rootNode: BinaryTreeNode[T]

  /**
   * Returns the left subtree of this binary tree. $LAZY $CX_1
   * @return The left subtree
   */
  def left: BinaryTree[T] = ofRootNode(rootNode.leftNode)

  /**
   * Returns the right subtree of this binary tree. $LAZY $CX_1
   * @return The right subtree
   */
  def right: BinaryTree[T] = ofRootNode(rootNode.rightNode)

  def root: T = rootNode.data

  /** Returns the maximal height of this tree. */ //TODO: a recursive-free version?
  def height: Int = fold(0)((l, r, _) => math.max(l, r) + 1)

  /** Returns the number of nodes in this tree. */
  def size: Int = rootNode.nodePreOrder.size

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
        case 0 => curr = curr.leftNode
        case 1 => curr = curr.rightNode
      }
      if (curr.isDummy) throw new KeyNotFoundException(i)
      x %= depth
      depth <<= 1
    }
    curr.data
  }

  def isDefinedAt(i: Int): Boolean = ???

  // HELPER FUNCTIONS

  def map[U](f: T => U): BinaryTree[U] = {
    class MappedNode(n: BinaryTreeNode[T]) extends BinaryTreeNode[U] {
      def data = f(n.data)
      def leftNode = new MappedNode(n.leftNode)
      def rightNode = new MappedNode(n.rightNode)
      def isDummy = n.isDummy
    }
    ofRootNode(new MappedNode(self.rootNode))
  }

  /** Folds a binary tree bottom-up. This is analogous to the sequence `foldRight` in that both
    * are catamorphisms on recursive structures.
    */
  def fold[U](z: U)(f: (U, U, T) => U): U = {
    val s = ArrayStack[U]()
    for (n <- rootNode.nodePostOrder) {
      if (n.leftNode .isDummy) s push z
      if (n.rightNode.isDummy) s push z
      val a = s.pop()
      val b = s.pop()
      s push f(a, b, n.data)
    }
    s.top
  }

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
  def zip[U](that: BinaryTree[U]) = {
    class ZippedNode(m: BinaryTreeNode[T], n: BinaryTreeNode[U]) extends BinaryTreeNode[(T, U)] {
      def leftNode = new ZippedNode(m.leftNode, n.leftNode)
      def rightNode = new ZippedNode(m.rightNode, n.rightNode)
      def data = (m.data, n.data)
      def isDummy = m.isDummy || n.isDummy
    }
    ofRootNode(new ZippedNode(self.rootNode, that.rootNode))
  }

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
  def preOrder = rootNode.nodePreOrder.map(_.data)

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
  def inOrder = rootNode.nodeInOrder.map(_.data)

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
  def reflect: BinaryTree[T] = {
    class ReflectedNode(n: BinaryTreeNode[T]) extends BinaryTreeNode[T] {
      def leftNode = new ReflectedNode(n.rightNode)
      def rightNode = new ReflectedNode(n.leftNode)
      def data = n.data
      def isDummy = n.isDummy
    }
    new AbstractBinaryTree[T] {
      def rootNode = new ReflectedNode(self.rootNode)
      override def reflect = self
    }
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
  def postOrder = rootNode.nodePostOrder.map(_.data)

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

  def leaves = rootNode.nodePreOrder.filter(_.isLeaf).map(_.data)

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
  def inverseKnuthTransform: OrderedTree[T] = new OrderedTree[T] {
    class InverseKnuthTransformedTreeNode(val node: BinaryTreeNode[T]) extends OrderedTreeNode[T] {
      override def isDummy = node.isDummy
      def children = node.leftNode.iterate(_.rightNode).takeUntil(_.isDummy).map(btn => new InverseKnuthTransformedTreeNode(btn))
      def data = node.data
    }
    def rootNode = new InverseKnuthTransformedTreeNode(self.rootNode)
    override def knuthTransform = self
  }

  def subtrees: BinaryTree[BinaryTree[T]] = {
    class SubtreeNode(n: BinaryTreeNode[T]) extends BinaryTreeNode[BinaryTree[T]] {
      def data = ofRootNode(n)
      def leftNode = new SubtreeNode(n.leftNode)
      def rightNode = new SubtreeNode(n.rightNode)
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


  def asTree: OrderedTree[T] = {
    class BinaryTreeAsTreeNode(n: BinaryTreeNode[T]) extends OrderedTreeNode[T] {
      def children = ListSeq(n.leftNode, n.rightNode).filter(_.notDummy).map(n => new BinaryTreeAsTreeNode(n))
      def data = n.data
      def isDummy = n.isDummy
    }
    OrderedTree.ofRootNode(new BinaryTreeAsTreeNode(self.rootNode))
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
      def leftNode = this
      def rightNode = this
      def isDummy = false
    }
  }

  object Empty extends BinaryTree[Nothing] {
    def rootNode = BinaryTreeNode.Dummy
    def unapply[T](bt: BinaryTree[T]) = bt.isEmpty
  }

  def ofRootNode[T](n: BinaryTreeNode[T]): BinaryTree[T] = new BinaryTree[T] {
    override def rootNode = n
  }

  implicit object ZipIdiom extends Idiom[BinaryTree] {
    def id[X](u: X): BinaryTree[X] = infinite(u)
    def liftedMap[X, Y](mx: BinaryTree[X])(mf: BinaryTree[X => Y]): BinaryTree[Y] = (mx zip mf).map { case (x, f) => f(x) }
    override def map[T, U](t: BinaryTree[T])(f: T => U): BinaryTree[U] = t map f
  }

  implicit object Comonad extends Comonad[BinaryTree] {
    def id[X](u: BinaryTree[X]) = u.root
    def extend[X, Y](wx: BinaryTree[X])(f: BinaryTree[X] => Y) = wx.subtrees map f
  }

}

abstract class AbstractBinaryTree[+T] extends BinaryTree[T]
