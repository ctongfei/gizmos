package poly.collection

import poly.collection.node._

/**
 * Represents an indexed binary tree (e.g. an array binary tree)
 * where fast random access using its canonical index is possible.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IndexedBinaryTree[+T] extends BiBinaryTree[T] { self =>

  class Node(val i: Int) extends BiBinaryTreeNode[T] {
    def isDummy = self.notContains(i)
    def data = self(i)
    def parent = new Node((i - 1) / 2)
    def left = new Node(2 * i + 1)
    def right = new Node(2 * i + 2)

    override def equals(that: Any) = that match {
      case that: Node => this.i == that.i
      case _ => false
    }

    override def hashCode = hashByRef(self) + i
  }


  def fastApply(i: Int): T

  final override def apply(i: Int) = fastApply(i)

  def contains(i: Int): Boolean

  def notContains(i: Int) = !contains(i)

  def rootNode = new Node(0)
}

abstract class AbstractIndexedBinaryTree[+T] extends AbstractBinaryTree[T] with IndexedBinaryTree[T]
