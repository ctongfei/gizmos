package poly.collection.node

import poly.collection._

/**
 * Represents a node that has a list of predecessor nodes.
 * @since 0.1.0
 */
trait BackwardNode[+T] {
  def data: T
  def pred: Enumerable[BackwardNode[T]]
}
