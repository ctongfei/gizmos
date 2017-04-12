package poly.collection.immut

import poly.collection._
import poly.collection.node._

/**
 * A functional binary tree.
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class FBinaryTree[+T] extends AbstractBinaryTree[T] with BinaryTreeNodeLike[T, FBinaryTree[T]] with BinaryTreeNode[T] { self =>

  import FBinaryTree._

  final def rootNode = self

  override def left: FBinaryTree[T] = leftNode

  override def right: FBinaryTree[T] = rightNode

  override def root = data

  override def toString = super[AbstractBinaryTree].toString
}

object FBinaryTree {

  case object Empty extends FBinaryTree[Nothing] {
    def data = throw new NoSuchElementException
    override def leftNode = Empty
    override def rightNode = Empty
    def isDummy = true
  }

  case class Cons[+T](h: T, l: FBinaryTree[T] = Empty, r: FBinaryTree[T] = Empty) extends FBinaryTree[T] {
    final def isDummy = false
    def leftNode = l
    def rightNode = r
    def data = h
  }

}
