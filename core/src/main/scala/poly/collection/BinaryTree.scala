package poly.collection

import poly.collection.exception._
import poly.collection.immut._
import poly.collection.node._

/**
 * Represents a binary tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BinaryTree[+T] extends BinaryTreeLike[T, BinaryTreeNode[T]] { self =>

  import BinaryTree._

  def dummy: BinaryTreeNode[T] = new BinaryTreeNode[T] {
    def data: T = throw new DummyNodeException
    def leftNode: BinaryTreeNode[T] = rootNode
    def rightNode: BinaryTreeNode[T] = rootNode
    def isDummy: Boolean = true
  }

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


  /**
   * $LAZY Zips two binary trees into one.
   * @example {{{
   *   ┌      a    ┐     ┌    1      ┐    ┌               ┐
   *   │     / \   │     │   / \     │    │     (a,1)     │
   *   │    b   c  │ zip │  2   3    │ == │     /   \     │
   *   │   / \     │     │     / \   │    │ (b,2)   (c,3) │
   *   └  d   e    ┘     └    4   5  ┘    └               ┘
   * }}}
   */
  def zip[U](that: BinaryTree[U]) = zipWith(that) { (t, u) => (t, u) }

  def zipWith[U, X](that: BinaryTree[U])(f: (T, U) => X) = {
    class ZippedWithNode(m: BinaryTreeNode[T], n: BinaryTreeNode[U]) extends BinaryTreeNode[X] {
      def leftNode = new ZippedWithNode(m.leftNode, n.leftNode)
      def rightNode = new ZippedWithNode(m.rightNode, n.rightNode)
      def data = f(m.data, n.data)
      def isDummy = m.isDummy || n.isDummy
    }
    ofRootNode(new ZippedWithNode(self.rootNode, that.rootNode))
  }

  def scan[U](z: U)(f: (U, U, T) => U) = ???

  def scanFromTop[U](z: U)(fLeft: (U, T) => U, fRight: (U, T) => U): BinaryTree[U] = {
    class FromTopScannedNode(node: BinaryTreeNode[T], u: U) extends BinaryTreeNode[U] {
      def leftNode = new FromTopScannedNode(node.leftNode, fLeft(u, node.leftNode.data))
      def rightNode = new FromTopScannedNode(node.rightNode, fRight(u, node.rightNode.data))
      def data = u
      def isDummy = node.isDummy
    }
    ofDummyNode(new FromTopScannedNode(self.dummy, z))
  }

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
      def children = n.children.map(n => new BinaryTreeAsTreeNode(n))
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

  def ofDummyNode[T](n: BinaryTreeNode[T]): BinaryTree[T] = new BinaryTree[T] {
    override def dummy = n
    def rootNode = n.rightNode
  }

  implicit object ZipApplicative extends Applicative[BinaryTree] {
    def pure[X](u: X): BinaryTree[X] = infinite(u)
    def ap[X, Y](mf: BinaryTree[X => Y])(mx: BinaryTree[X]): BinaryTree[Y] = (mx zip mf).map { case (x, f) => f(x) }
    override def map[T, U](t: BinaryTree[T])(f: T => U): BinaryTree[U] = t map f
  }

  implicit object Comonad extends Comonad[BinaryTree] {
    def extract[X](u: BinaryTree[X]) = u.root
    def coflatMap[X, Y](wx: BinaryTree[X])(f: BinaryTree[X] => Y) = wx.subtrees map f
    def map[A, B](fa: BinaryTree[A])(f: A => B): BinaryTree[B] = fa map f
  }

}

abstract class AbstractBinaryTree[+T] extends BinaryTree[T]
