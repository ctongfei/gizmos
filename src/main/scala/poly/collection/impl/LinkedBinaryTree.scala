package poly.collection.impl

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedBinaryTree[@specialized(Int, Double) T] {

  val dummy = new BinaryTreeNode[T](default[T])
  var root: BinaryTreeNode[T] = null
  var size = 0
  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def addRoot(x: T) = {
    root = new BinaryTreeNode[T](x, dummy, dummy, dummy)
    dummy.left = root
    dummy.right = root
    size = 1
  }

  def locate(i: Int): BinaryTreeNode[T] = ???


}
