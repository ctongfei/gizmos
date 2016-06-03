package poly.collection

import poly.collection.IndexedBinaryTree._
import poly.collection.node._

/**
 * Represents an indexed binary tree (e.g. an array binary tree)
 * where fast random access using its canonical index is possible.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IndexedBinaryTree[+T] extends BiBinaryTree[T] { self =>
  def fastApply(i: Int): T
  final override def apply(i: Int) = fastApply(i)

  def contains(i: Int): Boolean

  def notContains(i: Int) = !contains(i)

  def rootNode = new NodeProxy[T](self, 0)
}

object IndexedBinaryTree {

  class NodeProxy[+T](val tree: IndexedBinaryTree[T], val i: Int) extends BiBinaryTreeNode[T] {
    def isDummy = tree.notContains(i)
    def data = tree(i)
    def parent = new NodeProxy(tree, (i - 1) / 2)
    def left = new NodeProxy(tree, 2 * i + 1)
    def right = new NodeProxy(tree, 2 * i + 2)

    override def equals(that: Any) = that match {
      case that: NodeProxy[T] => (this.tree eq that.tree) && (this.i == that.i)
      case _ => false
    }

    override def hashCode = poly.algebra.Hashing.byRef.hash(tree) + i
  }

}

abstract class AbstractIndexedBinaryTree[+T] extends AbstractBinaryTree[T] with IndexedBinaryTree[T]
