package poly.collection.impl.bst

import poly.algebra._
import poly.algebra.ops._
import poly.collection.impl._

/**
 * Serves as a basis for self-balancing binary search trees.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinarySearchTree[T](implicit O: WeakOrder[T]) extends LinkedBinaryTree[T] {
  
  val order = O

  private[this] var _size = 0

  override def size = _size

  def locate(x: T): Node = {
    var c = rootNode
    while (c ne dummy) {
      if (x < c.data)
        c = c.left
      else if (x > c.data)
        c = c.right
      else return c
    }
    dummy
  }

  override def addRoot(x: T) = {
    super.addRoot(x)
    _size += 1
  }

  def add(x: T) = {
    var c = rootNode // current
    var p: Node = dummy // keeps track of the parent of c
    while (c ne dummy) {
      p = c
      if (x < p.data)
        c = c.left
      else c = c.right
    }
    c = new Node(x, p, dummy, dummy)
    if (p eq dummy)
      rootNode = c
    else if (x < p.data)
      p.left = c
    else p.right = c

    _size += 1
    c
  }

  def leftmost(x: Node): Node = {
    var l = x
    while (l.left ne dummy)
      l = l.left
    l
  }

  def rightmost(x: Node): Node = {
    var r = x
    while (r.right ne dummy)
      r = r.right
    r
  }

  /**
   * Performs a right rotation on the specific node ''p''.
   *     p             l
   *    / \           / \
   *   l   r   =>    a   p
   *  / \               / \
   * a  b              b   r
   */
  def rotateRight(p: Node) = {
    val c = p.left
    if ((c eq dummy) || (c.parent ne p))
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent ne dummy) { // p is not root
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.right ne dummy)
      c.right.parent = p
    c.parent = p.parent
    p.parent = c
    p.left = c.right
    c.right = p
  }

  def rotateLeft(p: Node) = {
    val c = p.right
    if ((c eq dummy) || (c.parent ne p))
      throw new IllegalArgumentException // c is leaf or malformed
    if (p.parent ne dummy) {
      if (p eq p.parent.left)
        p.parent.left = c
      else p.parent.right = c
    }
    if (c.left ne dummy)
      c.left.parent = p
    c.parent = p.parent
    p.parent = c
    p.right = c.left
    c.left = p
  }



}
