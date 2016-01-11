package poly.collection

import poly.collection.mut._

/**
 * Represents an undirected graph.
 * @author Tongfei Chen
 */
trait UndirectedGraph[K, +V, +E] extends BiGraph[K, V, E] { self =>

  import UndirectedGraph._

  override def node(i: K) = new Node(self, i)

  override def edge(i: K, j: K) = new Edge(self, i, j)

  def adjacentKeysOf(i: K): Iterable[K]
  def adjacentNodesOf(i: K) = adjacentKeysOf(i).map(j => node(j))
  def adjacentEdgesOf(i: K) = adjacentKeysOf(i).map(j => edge(i, j))

  def outgoingKeysOf(v: K): Iterable[K] = adjacentKeysOf(v)
  override def outgoingNodesOf(v: K) = adjacentNodesOf(v)
  override def outgoingEdgesOf(v: K) = adjacentEdgesOf(v)

  def incomingKeysOf(v: K): Iterable[K] = adjacentKeysOf(v)
  override def incomingNodesOf(v: K) = adjacentNodesOf(v)
  override def incomingEdgesOf(v: K) = adjacentEdgesOf(v)

  override def reverse = self

  override def edges = ???


}

object UndirectedGraph {

  type Node[K, +V] = BiGraph.Node[K, V]

  class Edge[K, +E](override val graph: UndirectedGraph[K, _, E], override val key1: K, override val key2: K) extends Graph.Edge(graph, key1, key2) with Set[K] {
    override def equals(that: Any) = that match {
        case that: UndirectedGraph.Edge[K, E] =>
          (this.key1 == that.key1 && this.key2 == that.key2) ||
            (this.key1 == that.key2 && this.key2 == that.key1)
        case _ => false
      }

    override def size = 2
    override def hashCode = key1.## ^ key2.##
    def contains(x: K) = x == key1 || x == key2
    def keys = ListSeq(key1, key2)
  }
}