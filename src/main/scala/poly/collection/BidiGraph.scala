package poly.collection

import poly.collection.node._
import poly.algebra.specgroup._

/**
 * Represents a bidirectional graph, i.e., a graph in which each
 * node's predecessors and successors can both be efficiently retrieved.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiGraph[@sp(Int) K, +E] extends Graph[K, E] { self =>

  import BidiGraph._

  def incomingKeySet(i: K): Set[K]

  override def node(i: K): GraphNode[K, E] = new NodeProxy(self, i)

  def incomingMap(i: K) = incomingKeySet(i) createMap { j => apply(j, i) }

  def incomingKeys(i: K) = incomingKeySet(i).elements
  def incomingNodes(i: K) = incomingKeys(i) map node
  def incomingArcs(i: K) = incomingKeys(i) map { j => arc(j, i) }

  def inDegree(i: K) = incomingKeySet(i).size

  def pred(i: K) = incomingKeys(i)

  // HELPER FUNCTIONS
  override def reverse: BidiGraph[K, E] = new BiGraphT.Reversed(self)

  override def map[F](f: E => F): BidiGraph[K, F] = new BiGraphT.Mapped(self, f)

  override def mapWithKeys[F](f: (K, K, E) => F): BidiGraph[K, F] = new BiGraphT.MappedWithKeys(self, f)

  //TODO: filterKeys, zip, zipWith

  override def asMultimap: BiMultimap[K, K] = new AbstractBiMultimap[K, K] {
    def valueSet = self.keySet
    def keySet = self.keySet
    def invert(k: K) = self.incomingKeySet(k)
    def apply(k: K) = self.outgoingKeySet(k)
  }

}

object BidiGraph {
  class NodeProxy[K, +E](override val graph: BidiGraph[K, E], override val key: K) extends Graph.NodeProxy[K, E](graph, key) { //TODO: BiNode
    def incomingMap = graph.incomingMap(key)
    def pred = graph.pred(key) map { i => new NodeProxy(graph, i) }
    override def succ = graph.succ(key) map { i => new NodeProxy(graph, i) }
    def incomingKeySet = graph.incomingKeySet(key)
  }

}

abstract class AbstractBidiGraph[K, +E] extends AbstractGraph[K, E] with BidiGraph[K, E]

private[poly] object BiGraphT {

  class Reversed[K, +E](self: BidiGraph[K, E]) extends AbstractBidiGraph[K, E] {
    def keySet = self.keySet
    override def reverse = self
    def outgoingKeySet(i: K) = self.incomingKeySet(i)
    def incomingKeySet(i: K) = self.outgoingKeySet(i)
    def ?(i: K, j: K) = self ? (j, i)
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def apply(i: K, j: K) = self.apply(i, j)
  }

  class Mapped[K, E, F](self: BidiGraph[K, E], f: E => F) extends GraphT.Mapped(self, f) with BidiGraph[K, F] {
    def incomingKeySet(i: K) = self.incomingKeySet(i)
  }

  class MappedWithKeys[K, E, F](self: BidiGraph[K, E], f: (K, K, E) => F) extends GraphT.MappedWithKeys(self, f) with BidiGraph[K, F] {
    def incomingKeySet(i: K) = self.incomingKeySet(i)
  }



}