package poly.collection.impl

import poly.collection._
import poly.collection.node._
import poly.collection.tree._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedBinaryTree[T] extends BinaryTree[T]
{

  class Node (
    var data: T,
    var left: Node = null,
    var right: Node = null,
    var parent: Node = null
  ) extends BiBinaryTreeNode[T]

  val dummy: Node = new Node(default[T])
  var rootNode: Node = null

  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def addRoot(x: T) = {
    rootNode = new Node(x, dummy, null, null)
    dummy.right = rootNode
  }

  /** Locates the binary tree node at the given index. */
  def findNode(i: Int): Node = {
    var x = i + 1
    var curr = dummy
    var depth = 1
    while (depth <= x) {
      x / depth match {
        case 0 => curr = curr.left
        case 1 => curr = curr.right
      }
      x %= depth
      depth <<= 1
    }
    curr
  }

  def update(i: Int, x: T) = findNode(i).data = x

}

object LinkedBinaryTree {


}
