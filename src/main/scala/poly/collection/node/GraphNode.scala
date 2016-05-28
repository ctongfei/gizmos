package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait GraphNode[K, +E] extends ForwardNode[K] {

  def key: K

  /** Returns the data on this node. */
  final def data: K = key

  def outgoingKeySet: Set[K]

  def outgoingMap: Map[K, E]

  def succ: Iterable[GraphNode[K, E]]

  def isDummy: Boolean
}

trait GraphArc[K, +E] {

  def source: K
  def target: K

  def sourceNode: GraphNode[K, E]
  def targetNode: GraphNode[K, E]

  def data: E

}

trait GraphEdge[K, +E] {
  def key1: K
  def key2: K
  def node1: GraphNode[K, E]
  def node2: GraphNode[K, E]

  def data: E
}
