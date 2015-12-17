package poly.collection.impl

import poly.collection._
import poly.collection.node._

/**
 * A linked binary tree with parent pointer.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class LinkedBinaryTree[T] extends BinaryTree[T] {

  type Node = LinkedBinaryTree.Node[T]

  final val dummy: Node = new Node(default[T], dummy, dummy, dummy) {
    override def isDummy = true
  }
  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  var rootNode: Node = dummy

  def addRoot(x: T) = {
    if (rootNode.isDummy) {
      rootNode = new Node(x, dummy, dummy, dummy)
      dummy.right = rootNode
    }
  }

  /** Locates the binary tree node at the given index. */
  def findNode(i: Int): Node = {
    var x = i + 1
    var curr: Node = dummy
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
    while (l.left.notDummy)
      l = l.left
    l
  }

  def rightmost(x: Node): Node = {
    var r = x
    while (r.right.notDummy)
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
    if (c.isDummy || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent.notDummy) { // p is not root
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.right.notDummy)
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
    if (c.isDummy || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent.notDummy) {
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.left.notDummy)
      c.left.parent = p
    c.parent = p.parent
    p.parent = c
    p.right = c.left
    c.left = p
  }

  def inOrderSuccessor(t: Node): Node = {
    if (t.right.notDummy) leftmost(t.right)
    else {
      var p = t.parent
      var c = t
      while (p.notDummy && c == p.right) {
        c = p
        p = p.parent
      }
      p
    }
  }

  def inOrderPredecessor(t: Node): Node = {
    if (t.left.notDummy) rightmost(t.left)
    else {
      var p = t.parent
      var c = t
      while (p.notDummy && c == p.left) {
        c = p
        p = p.parent
      }
      p
    }
  }

}

object LinkedBinaryTree {

  class Node[T] (
    var data: T,
    var left: Node[T],
    var right: Node[T],
    var parent: Node[T]
  ) extends BiBinaryTreeNode[T] {
    def isDummy = false
  }

}
