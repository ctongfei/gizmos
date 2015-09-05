package poly.collection

import poly.collection.node._
import poly.util.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * vertex's predecessors and successors can be retrieved efficiently.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait BiGraph[@sp(i) K, +V, +E] extends Graph[K, V, E] { self =>

  def incomingKeysOf(i: K): Iterable[K]
  def incomingVerticesOf(i: K): Iterable[Vertex] = incomingKeysOf(i).map(j => new Vertex(j))
  def incomingEdgesOf(i: K): Iterable[Edge] = incomingKeysOf(i).map(j => Edge(j, i))
  def inDegree(i: K) = incomingKeysOf(i).size

  override def outgoingVerticesOf(i: K): Iterable[Vertex] = outgoingKeysOf(i).map(j => new Vertex(j))

  // HELPER FUNCTIONS
  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BiGraph[K, V, E] = new AbstractBiGraph[K, V, E] {
    override def reverse = self
    def keySet: Set[K] = self.keySet
    def containsVertex(i: K): Boolean = self.containsVertex(i)
    def containsEdge(i: K, j: K) = self.containsEdge(j, i)
    def incomingKeysOf(i: K) = self.outgoingKeysOf(i)
    def outgoingKeysOf(i: K) = self.incomingKeysOf(i)
    def apply(i: K): V = self.apply(i)
    def apply(i: K, j: K): E = self.apply(j, i)
  }

  class Vertex(override val key: K) extends super.Vertex(key) with BiNode[V] {
    def pred = self.incomingVerticesOf(key)
    override def succ = self.outgoingVerticesOf(key)
  }

}

abstract class AbstractBiGraph[@sp(i) K, +V, +E] extends BiGraph[K, V, E]
