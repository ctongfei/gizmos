package poly.collection

import poly.algebra.specgroup._
import poly.collection.mut._

/**
 * Represents an undirected graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait UndirectedGraph[@sp(Int) K, +V, +E] extends BiGraph[K, V, E] { self =>

  import UndirectedGraph._

  override def node(i: K) = new Node(self, i)

  override def arc(i: K, j: K) = new BiGraph.Arc(self, i, j)

  def edge(i: K, j: K) = new UndirectedEdge(self, i, j)

  def adjacentMapOf(i: K): Map[K, E]

  def adjacentKeysOf(i: K) = adjacentMapOf(i).keys
  def adjacentNodesOf(i: K) = adjacentKeysOf(i).map(j => node(j))
  def adjacentEdgesOf(i: K) = adjacentKeysOf(i).map(j => arc(i, j))

  def outgoingMapOf(v: K) = adjacentMapOf(v)
  override def outgoingNodesOf(v: K) = adjacentNodesOf(v)
  override def outgoingArcsOf(v: K) = adjacentEdgesOf(v)

  def incomingMapOf(v: K) = adjacentMapOf(v)
  override def incomingNodesOf(v: K) = adjacentNodesOf(v)
  override def incomingArcsOf(v: K) = adjacentEdgesOf(v)

  override def reverse = self

  override def arcs = ???


}

object UndirectedGraph {

  type Node[K, +V] = BiGraph.Node[K, V]

  class UndirectedEdge[K, +E](val graph: UndirectedGraph[K, _, E], val key1: K, val key2: K) extends Keyed[K] {
    override def equals(that: Any) = that match {
        case that: UndirectedGraph.UndirectedEdge[K, E] =>
          (this.key1 == that.key1 && this.key2 == that.key2) ||
            (this.key1 == that.key2 && this.key2 == that.key1)
        case _ => false
      }

    def eqOnKeys = graph.eqOnKeys

    override def hashCode = key1.## ^ key2.##
    def contains(x: K) = x == key1 || x == key2
    def keys = ListSeq(key1, key2)
  }
}
