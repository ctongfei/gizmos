package poly.collection.tree

import poly.algebra.hkt._
import poly.collection._
import poly.collection.exception._
import poly.collection.node._

/**
 * Represents a binary tree.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BinaryTree[+T] extends (Int =?> T) { self =>

  def rootNode: BinaryTreeNode[T]

  def root: T = rootNode.data

  /** Returns the maximal depth of this tree. */
  def depth: Int = ???

  /** Returns the number of nodes in this tree. */
  def size: Int = rootNode.size

  /**
   * Returns the ''i''th node of this binary tree.
   * The ''i''th is defined as follows:
   *  - The index of the root node is 0;
   *  - The left child of node ''i'' is 2''i'' + 1;
   *  - The right child of node ''i'' is 2''i'' + 2.
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
      if (curr eq null) throw new NoSuchElementException
      x %= depth
      depth <<= 1
    }
    curr.data
  }

  def isDefinedAt(i: Int): Boolean = ???


  /**
   * Returns the left subtree of this binary tree.
   * Deferred execution.
   * @return The left subtree
   */
  def left: BinaryTree[T] = BinaryTree.ofNode(rootNode.left)

  /**
   * Returns the right subtree of this binary tree.
   * @return
   */
  def right: BinaryTree[T] = BinaryTree.ofNode(rootNode.right)

  def map[U](f: T => U): BinaryTree[U] = BinaryTree.ofNode(rootNode.map(f))

  def zip[U](that: BinaryTree[U]): BinaryTree[(T, U)] = BinaryTree.ofNode(rootNode zip that.rootNode)

  def preOrder = rootNode.preOrder
  def inOrder = rootNode.inOrder
  def postOrder = rootNode.postOrder

}

object BinaryTree {

  @inline def parentIndex(i: Int) = (i - 1) / 2
  @inline def leftChildIndex(i: Int) = 2 * i + 1
  @inline def rightChildIndex(i: Int) = 2 * i + 2

  def ofNode[T](n: BinaryTreeNode[T]): BinaryTree[T] = new BinaryTree[T] {
    def rootNode = n
  }

  implicit object Functor extends Functor[BinaryTree] {
    def map[T, U](t: BinaryTree[T])(f: T => U): BinaryTree[U] = t.map(f)
  }

}