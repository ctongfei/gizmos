package poly.collection

import poly.collection.node._
import poly.util.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * vertex's predecessors and successors can be retrieved efficiently.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiGraph[@sp(i) I, +V, +E] extends Graph[I, V, E] { self =>

  def incomingKeysOf(i: I): Enumerable[I]
  def incomingVerticesOf(i: I): Enumerable[Vertex] = incomingKeysOf(i).map(j => new Vertex(j))
  def incomingEdgesOf(i: I): Enumerable[Edge] = incomingKeysOf(i).map(j => Edge(j, i))
  def inDegree(i: I) = incomingKeysOf(i).size

  override def outgoingVerticesOf(i: I): Enumerable[Vertex] = outgoingKeysOf(i).map(j => new Vertex(j))

  // HELPER FUNCTIONS
  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BiGraph[I, V, E] = new BiGraph[I, V, E] {
    override def reverse = self
    def keySet: Set[I] = self.keySet
    def containsVertex(i: I): Boolean = self.containsVertex(i)
    def containsEdge(i: I, j: I) = self.containsEdge(j, i)
    def incomingKeysOf(i: I) = self.outgoingKeysOf(i)
    def outgoingKeysOf(i: I) = self.incomingKeysOf(i)
    def apply(i: I): V = self.apply(i)
    def apply(i: I, j: I): E = self.apply(j, i)
  }

  class Vertex(override val key: I) extends super.Vertex(key) with BiNode[V] {
    def pred = self.incomingVerticesOf(key)
    override def succ = self.outgoingVerticesOf(key)
  }

}
