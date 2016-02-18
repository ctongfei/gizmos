package poly.collection.node

/**
 * @author Tongfei Chen
 */
trait KeyedNodeLike[K, +T, +N <: KeyedNodeLike[K, T, N]] extends NodeLike[T, KeyedNodeLike[K, T, N]] { self: N =>

  def key: K

}
