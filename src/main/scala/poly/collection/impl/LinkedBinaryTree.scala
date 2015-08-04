package poly.collection.impl

import poly.collection._
import poly.collection.node._

/**
 * A linked binary tree with parent pointer.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedBinaryTree[T] extends BinaryTree[T] {

  class Node (
    var data: T,
    var left: Node = dummy,
    var right: Node = dummy,
    var parent: Node = dummy
  ) extends BiBinaryTreeNode[T]

  val dummy: Node = new Node(default[T])
  var rootNode: Node = dummy

  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def addRoot(x: T) = {
    if (rootNode == dummy) {
      rootNode = new Node(x, parent = dummy)
      dummy.right = rootNode
    }
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

  def leftmost(x: Node): Node = {
    var l = x
    while (l.left != dummy)
      l = l.left
    l
  }

  def rightmost(x: Node): Node = {
    var r = x
    while (r.right != dummy)
      r = r.right
    r
  }


  /**
   * Performs a right rotation on the specific node ''p''.
   * {{{
   *     p             l
   *    / \           / \
   *   l   r   =>    a   p
   *  / \               / \
   * a  b              b   r
   * }}}
   */
  def rotateRight(p: Node) = {
    val c = p.left
    if (c == dummy || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent != dummy) { // p is not root
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.right != dummy)
      c.right.parent = p
    c.parent = p.parent
    p.parent = c
    p.left = c.right
    c.right = p
  }

  /** Performs a left rotation of the specific node ''p''.
    * {{{
    *     p              r
    *    / \            / \
    *   l   r    =>    p   b
    *      / \        / \
    *     a   b      l   a
    * }}}
    */
  def rotateLeft(p: Node) = {
    val c = p.right
    if (c == null || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent != dummy) {
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.left != dummy)
      c.left.parent = p
    c.parent = p.parent
    p.parent = c
    p.right = c.left
    c.left = p
  }

  def inOrderSuccessor(t: Node): Node = {
    if (t.right != dummy) leftmost(t.right)
    else {
      var p = t.parent
      var c = t
      while (p != dummy && c == p.right) {
        c = p
        p = p.parent
      }
      p
    }
  }

  def inOrderPredecessor(t: Node): Node = {
    if (t.left != dummy) rightmost(t.left)
    else {
      var p = t.parent
      var c = t
      while (p != dummy && c == p.left) {
        c = p
        p = p.parent
      }
      p
    }
  }

}

object LinkedBinaryTree {


}
