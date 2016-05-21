package poly.collection.impl.bst

import poly.collection.node._

/**
 * @author Tongfei Chen
 */
trait BinarySearchTreeNodeLike[K, N <: BinarySearchTreeNodeLike[K, N]] extends BiBinaryTreeNodeLike[K, N] { self: N =>

  var left: N = _

  var right: N = _

  var parent: N = _

  def key: K

}
