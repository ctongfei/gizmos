package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait GraphNode[K, +E] extends ForwardNodeLike[K, GraphNode[K, E]] {

  def key: K

  /** Returns the data on this node. */
  final def data: K = key

  def outgoingKeySet: Set[K]

  def outgoingMap: Map[K, E]

  def succ: Iterable[GraphNode[K, E]]

  def isDummy: Boolean
}
