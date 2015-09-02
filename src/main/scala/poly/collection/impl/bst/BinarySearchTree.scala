package poly.collection.impl.bst

import poly.algebra._
import poly.algebra.ops._
import poly.collection.impl._

/**
 * Serves as a basis for self-balancing binary search trees.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinarySearchTree[T](implicit val order: WeakOrder[T]) extends LinkedBinaryTree[T] {

  private[this] var _size = 0

  override def size = _size

  def locate(x: T): Node = {
    var c = rootNode
    while (c.notDummy) {
      x >?< c.data match {
        case 0 => return c
        case cmp if cmp < 0 => c = c.left
        case cmp if cmp > 0 => c = c.right
      }
    }
    null
  }

  override def addRoot(x: T) = {
    super.addRoot(x)
    _size += 1
  }

  def add(x: T): Node = {
    var c = rootNode // current
    var p: Node = dummy // keeps track of the parent of c
    while (c.notDummy) {
      p = c

      x >?< p.data match {
        case 0 => return c // already in the BST; return
        case cmp if cmp < 0 => c = c.left
        case cmp if cmp > 0 => c = c.right
      }
    }
    c = new Node(x, parent = p)
    if (p.isDummy)
      rootNode = c
    else if (x < p.data)
      p.left = c
    else p.right = c

    _size += 1
    c
  }


}
