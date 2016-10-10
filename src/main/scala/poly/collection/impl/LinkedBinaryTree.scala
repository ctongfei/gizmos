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

  final override val dummy: Node = new Node(default[T], dummy, dummy, dummy) {
    override def isDummy = true
  }
  dummy.leftNode = dummy
  dummy.rightNode = dummy
  dummy.parent = dummy

  var rootNode: Node = dummy

  def addRoot(x: T) = {
    if (rootNode.isDummy) {
      rootNode = new Node(x, dummy, dummy, dummy)
      dummy.rightNode = rootNode
    }
  }

  /** Locates the binary tree node at the given index. */
  def findNode(i: Int): Node = { //TODO: Wrong!
    var x = i + 1
    var curr: Node = dummy
    var depth = 1
    while (depth <= x) {
      x / depth match {
        case 0 => curr = curr.leftNode
        case 1 => curr = curr.rightNode
      }
      x %= depth
      depth *= 2
    }
    curr
  }

  def update(i: Int, x: T) = findNode(i).data = x

  def leftmost(x: Node): Node = {
    var l = x
    while (l.leftNode.notDummy)
      l = l.leftNode
    l
  }

  def rightmost(x: Node): Node = {
    var r = x
    while (r.rightNode.notDummy)
      r = r.rightNode
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
    val c = p.leftNode
    if (c.isDummy || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent.notDummy) { // p is not root
      if (p eq p.parent.leftNode)
        p.parent.leftNode = c
      else p.parent.rightNode = c
    }
    if (c.rightNode.notDummy)
      c.rightNode.parent = p
    c.parent = p.parent
    p.parent = c
    p.leftNode = c.rightNode
    c.rightNode = p
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
    val c = p.rightNode
    if (c.isDummy || c.parent != p)
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent.notDummy) {
      if (p eq p.parent.leftNode)
        p.parent.leftNode = c
      else p.parent.rightNode = c
    }
    if (c.leftNode.notDummy)
      c.leftNode.parent = p
    c.parent = p.parent
    p.parent = c
    p.rightNode = c.leftNode
    c.leftNode = p
  }

  def inOrderSuccessor(t: Node): Node = {
    if (t.rightNode.notDummy) leftmost(t.rightNode)
    else {
      var p = t.parent
      var c = t
      while (p.notDummy && c == p.rightNode) {
        c = p
        p = p.parent
      }
      p
    }
  }

  def inOrderPredecessor(t: Node): Node = {
    if (t.leftNode.notDummy) rightmost(t.leftNode)
    else {
      var p = t.parent
      var c = t
      while (p.notDummy && c == p.leftNode) {
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
                  var leftNode: Node[T],
                  var rightNode: Node[T],
                  var parent: Node[T]
  ) extends BidiBinaryTreeNode[T] {
    def isDummy = false
  }

}

