package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ArrayBinaryTree[T] private(private[poly] val data: ResizableArray[Option[T]]) {

  def apply(i: Int) = data(i).get

  def update(i: Int, x: T) = data(i) = Some(x)

  def insertAt(i: Int, x: T, direction: Boolean) = ???

  def subtree(i: Int) = ???

}

class ArrayBinaryTreeNode[T](val tree: ArrayBinaryTree[T], val i: Int) extends BidiNode[T] {

  def data = tree.data(i).get
  def data_=(x: T) = tree.data(i) = Some(x)

  def parent = new ArrayBinaryTreeNode[T](tree, (i - 1) / 2)
  def left = new ArrayBinaryTreeNode[T](tree, 2 * i + 1)
  def right = new ArrayBinaryTreeNode[T](tree, 2 * i + 1)

  def ancestors = ListSeq(parent)
  def descendants = ListSeq(left, right)

}
