package poly.collection

import poly.collection.node._
import poly.algebra.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * node's predecessors and successors can be efficiently retrieved.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiGraph[@sp(Int) K, +V, +E] extends Graph[K, V, E] { self =>

  import BiGraph._

  override def node(i: K) = new Node(self, i)
  override def arc(i: K, j: K) = new Arc(self, i, j)

  def incomingMapOf(i: K): Map[K, E]

  def incomingKeysOf(i: K) = incomingMapOf(i).keys
  def incomingNodesOf(i: K) = incomingMapOf(i).keys.map(node)
  def incomingArcsOf(i: K) = incomingKeysOf(i).map(j => arc(j, i))
  def inDegree(i: K) = incomingKeysOf(i).size

  // HELPER FUNCTIONS
  override def reverse: BiGraph[K, V, E] = new AbstractBiGraph[K, V, E] {
    override def reverse = self
    def keySet = self.keySet
    override def containsArc(i: K, j: K) = self.containsArc(j, i)
    def incomingMapOf(i: K) = self.outgoingMapOf(i)
    def outgoingMapOf(i: K) = self.incomingMapOf(i)
    def apply(i: K): V = self.apply(i)
    def apply(i: K, j: K): E = self.apply(j, i)
  }

}

object BiGraph {
  class Node[K, +V](override val graph: BiGraph[K, V, _], override val key: K) extends Graph.Node[K, V](graph, key) with BiNodeLike[V, Node[K, V]] {
    def pred = graph.incomingNodesOf(key)
    override def succ = ???
  }

  type Arc[K, +E] = Graph.Arc[K, E]
}

abstract class AbstractBiGraph[K, +V, +E] extends AbstractGraph[K, V, E] with BiGraph[K, V, E]
