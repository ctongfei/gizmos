package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryTreeNode[@specialized(Int, Double) T] (
  var data: T,
  var parent: BinaryTreeNode[T] = null,
  var left: BinaryTreeNode[T] = null,
  var right: BinaryTreeNode[T] = null
) extends Node[T] {

  def ancestors = ListSeq(parent)
  def descendants = ListSeq(left, right)

}
