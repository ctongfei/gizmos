package poly.collection

import poly.collection.node._
import poly.algebra.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * node's predecessors and successors can be efficiently retrieved.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiGraph[@sp(Int) K, +E] extends Graph[K, E] { self =>

  import BiGraph._

  def incomingKeySet(i: K): Set[K]

  override def node(i: K): GraphNode[K, E] = new NodeProxy(self, i)

  def incomingMap(i: K) = incomingKeySet(i) createMapBy { j => apply(j, i) }

  def incomingKeys(i: K) = incomingKeySet(i).elements
  def incomingNodes(i: K) = incomingKeys(i) map node
  def incomingArcs(i: K) = incomingKeys(i) map { j => arc(j, i) }

  def inDegree(i: K) = incomingKeySet(i).size

  def pred(i: K) = incomingKeys(i)

  // HELPER FUNCTIONS
  override def reverse: BiGraph[K, E] = new BiGraphT.Reversed(self)

  //TODO: mapArcs, filterKeys, zip, ...

}

object BiGraph {
  class NodeProxy[K, +E](override val graph: BiGraph[K, E], override val key: K) extends Graph.NodeProxy[K, E](graph, key) { //TODO: BiNode
    def incomingMap = graph.incomingMap(key)
    def pred = graph.pred(key) map { i => new NodeProxy(graph, i) }
    override def succ = graph.succ(key) map { i => new NodeProxy(graph, i) }
    def incomingKeySet = graph.incomingKeySet(key)
  }

}

abstract class AbstractBiGraph[K, +E] extends AbstractGraph[K, E] with BiGraph[K, E]

private[poly] object BiGraphT {

  class Reversed[K, +E](self: BiGraph[K, E]) extends AbstractBiGraph[K, E] {
    override def reverse = self
    def outgoingKeySet(i: K) = self.incomingKeySet(i)
    def incomingKeySet(i: K) = self.outgoingKeySet(i)
    def keys = self.keys
    def ?(i: K, j: K) = self ? (j, i)
    def containsKey(i: K) = self.containsKey(i)
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def apply(i: K, j: K) = self.apply(i, j)
    def eqOnKeys = self.eqOnKeys
  }


}
