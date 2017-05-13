package poly.collection.impl.bst

import poly.collection._
import poly.collection.node._

/**
 * @author Tongfei Chen
 */
abstract class BinarySearchTree[T, N >: Null <: BinaryTreeNodeLike[T, N]] extends BinaryTreeLike[T, N] {

  implicit def order: Order[T]

  def locate(x: T): N = {
    var c = rootNode
    while (c.notDummy) {
      order.compare(x, c.data) match {
        case r if r < 0 => c = c.leftNode
        case 0          => return c
        case r if r > 0 => c = c.rightNode
      }
    }
    c
  }

  def leftmost(x: N): N = {
    var l = x
    while (l.leftNode.notDummy)
      l = l.leftNode
    l
  }

  def rightmost(x: N): N = {
    var r = x
    while (r.rightNode.notDummy)
      r = r.rightNode
    r
  }


}
