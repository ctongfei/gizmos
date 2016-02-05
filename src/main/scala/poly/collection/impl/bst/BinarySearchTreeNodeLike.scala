package poly.collection.impl.bst

/**
 * @author Tongfei Chen
 */
trait BinarySearchTreeNodeLike[K, N <: BinarySearchTreeNodeLike[K, N]] { self: N =>

  var left: N = _

  var right: N = _

  var parent: N = _

  def key: K

  def isDummy: Boolean

  def notDummy = !isDummy

}
