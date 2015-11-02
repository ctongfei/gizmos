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

  def incomingKeysOf(i: K): Iterable[K]
  def incomingNodesOf(i: K): Iterable[Node] = incomingKeysOf(i).map(j => new Node(j))
  def incomingEdgesOf(i: K): Iterable[Edge] = incomingKeysOf(i).map(j => Edge(j, i))
  def inDegree(i: K) = incomingKeysOf(i).size

  override def outgoingNodesOf(i: K): Iterable[Node] = outgoingKeysOf(i).map(j => new Node(j))

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

  class Node(override val key: K) extends super.Node(key) with BiNode[V] {
    def pred = self.incomingNodesOf(key)
    override def succ = self.outgoingNodesOf(key)
  }

}

abstract class AbstractBiGraph[@sp(i) K, +V, +E] extends BiGraph[K, V, E]
