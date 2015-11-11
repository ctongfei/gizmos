package poly.collection

import poly.collection.mut._

/**
 * Represents an undirected graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait UndirectedGraph[K, +V, +E] extends BiGraph[K, V, E] { self =>

  import UndirectedGraph._

  override def node(i: K) = new Node(self, i)

  override def edge(i: K, j: K) = new Edge(self, i, j)

  def incidentKeysOf(i: K): Iterable[K]
  def incidentNodesOf(i: K) = incidentKeysOf(i).map(j => node(j))
  def incidentEdgesOf(i: K) = incidentKeysOf(i).map(j => edge(i, j))

  def outgoingKeysOf(v: K): Iterable[K] = incidentKeysOf(v)
  override def outgoingNodesOf(v: K) = incidentNodesOf(v)
  override def outgoingEdgesOf(v: K) = incidentEdgesOf(v)

  def incomingKeysOf(v: K): Iterable[K] = incidentKeysOf(v)
  override def incomingNodesOf(v: K) = incidentNodesOf(v)
  override def incomingEdgesOf(v: K) = incidentEdgesOf(v)

  override def reverse = self

  override def edges = ???


}

object UndirectedGraph {

  type Node[K, +V] = BiGraph.Node[K, V]

  class Edge[K, +E](override val graph: UndirectedGraph[K, _, E], override val key1: K, override val key2: K) extends Graph.Edge(graph, key1, key2) with Set[K] {
    def equivOnKey = ??? //TODO:!!!
    override def equals(that: Any) = that match {
        case that: UndirectedGraph.Edge[K, E] =>
          (this.key1 == that.key1 && this.key2 == that.key2) ||
            (this.key1 == that.key2 && this.key2 == that.key1)
        case _ => false
      }

    def size = 2
    override def hashCode = key1.## ^ key2.##
    def contains(x: K) = x == key1 || x == key2
    def elements = ListSeq(key1, key2)
  }
}