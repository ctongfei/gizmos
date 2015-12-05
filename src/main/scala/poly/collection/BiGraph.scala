package poly.collection

import poly.collection.node._
import poly.util.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * vertex's predecessors and successors can be efficiently retrieved.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait BiGraph[@sp(i) K, +V, +E] extends Graph[K, V, E] { self =>

  import BiGraph._

  override def node(i: K) = new Node(self, i)
  override def edge(i: K, j: K) = new Edge(self, i, j)

  def incomingKeysOf(i: K): Iterable[K]
  def incomingNodesOf(i: K) = incomingKeysOf(i).map(j => node(j))
  def incomingEdgesOf(i: K) = incomingKeysOf(i).map(j => edge(j, i))
  def inDegree(i: K) = incomingKeysOf(i).size

  override def outgoingNodesOf(i: K) = outgoingKeysOf(i).map(j => node(j))

  // HELPER FUNCTIONS
  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BiGraph[K, V, E] = new AbstractBiGraph[K, V, E] {
    override def reverse = self
    def keySet: Set[K] = self.keySet
    def containsNode(i: K): Boolean = self.containsNode(i)
    def containsEdge(i: K, j: K) = self.containsEdge(j, i)
    def incomingKeysOf(i: K) = self.outgoingKeysOf(i)
    def outgoingKeysOf(i: K) = self.incomingKeysOf(i)
    def apply(i: K): V = self.apply(i)
    def apply(i: K, j: K): E = self.apply(j, i)
  }

}

object BiGraph {
  class Node[K, +V](override val graph: BiGraph[K, V, _], override val key: K) extends Graph.Node(graph, key) with BiNode[V] {
    def pred = graph.incomingNodesOf(key)
    override def succ = graph.outgoingNodesOf(key)
  }

  type Edge[K, +E] = Graph.Edge[K, E]
}

abstract class AbstractBiGraph[K, +V, +E] extends AbstractGraph[K, V, E] with BiGraph[K, V, E]
