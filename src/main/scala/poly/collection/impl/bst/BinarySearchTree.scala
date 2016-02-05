package poly.collection.impl.bst

import poly.algebra._
import poly.algebra.ops._
import poly.collection._

/**
 * Serves as a basis for self-balancing binary search trees.
 * @author Tongfei Chen
 */
trait BinarySearchTree[K, N >: Null <: BinarySearchTreeNodeLike[K, N]] {

  implicit def orderOnKey: WeakOrder[K]

  var dummy: N

  def rootNode: N = dummy.right
  def rootNode_=(n: N) = dummy.right = n

  def locate(x: K): N = {
    var c = rootNode
    while (c.notDummy) {
      x >?< c.key match {
        case 0 => return c
        case cmp if cmp < 0 => c = c.left
        case cmp if cmp > 0 => c = c.right
      }
    }
    dummy
  }

  def addNode(x: N): Unit = {
    var c = rootNode // current
    var p: N = dummy // keeps track of the parent of c
    while (c.notDummy) {
      p = c

      x.key >?< c.key match {
        case 0 => return // already in the BST; return
        case cmp if cmp < 0 => c = c.left
        case cmp if cmp > 0 => c = c.right
      }
    }
    x.left = dummy
    x.right = dummy
    if (p == dummy)
      rootNode = c
    else if (x.key < p.key)
      p.left = c
    else p.right = c
  }






}
