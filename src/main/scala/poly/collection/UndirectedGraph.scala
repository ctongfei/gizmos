package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents an undirected graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait UndirectedGraph[@sp(Int) K, +E] extends BiGraph[K, E] { self =>

  import UndirectedGraph._

  def edge(i: K, j: K) = new EdgeProxy(self, i, j)

  def edges = {
    val visited = AutoSet[K]()
    keys flatMap { i: K =>
      visited += i
      adjacentEdges(i) filter { e => visited notContains e.key2 } //TODO: maybe wrong?
    }
  }

  def edgeMap: Map[UPair[K], E] = new AbstractMap[UPair[K], E] {
    def ?(k: UPair[K]) = self ? (k._1, k._2)
    def keys = edges map { e => UPair(e.key1, e.key2)(self.eqOnKeys) }
    def containsKey(k: UPair[K]) = self.containsArc(k._1, k._2)
    def apply(k: UPair[K]) = self(k._1, k._2)
    def eqOnKeys = UPair.Eq(self.eqOnKeys)
  }

  def adjacentKeySet(i: K): Set[K]

  def adjacentMap(i: K) = adjacentKeySet(i) createMap { j => apply(i, j) }
  def adjacentKeys(i: K) = adjacentKeySet(i).elements
  def adjacentNodes(i: K) = adjacentKeys(i) map node
  def adjacentEdges(i: K) = adjacentKeys(i) map { j => edge(i, j) }

  def incomingKeySet(i: K) = adjacentKeySet(i)
  def outgoingKeySet(i: K) = adjacentKeySet(i)

  override def reverse = self

  override def map[F](f: E => F): UndirectedGraph[K, F] = ???

  def mapWithKeys[F](f: (UPair[K], E) => F) = ???

  def zip[F](that: UndirectedGraph[K, F]) = zipWith(that) { case (e, f) => (e, f) }

  def zipWith[F, H](that: UndirectedGraph[K, F])(f: (E, F) => H): UndirectedGraph[K, H] = ???

  override def asMultimap: BiMultimap[K, K] = ???

  override def toString = "{" + edges.map(e => s"${e.key1} <-(${e.data})-> ${e.key2}").buildString(", ") + "}"
}

object UndirectedGraph {

  class EdgeProxy[K, +E](val graph: UndirectedGraph[K, E], val key1: K, val key2: K) extends GraphEdge[K, E] {
    def contains(x: K) = x == key1 || x == key2
    def keys = ListSeq(key1, key2)
    def data = graph(key1, key2)
    def node1 = graph.node(key1)
    def node2 = graph.node(key2)
    override def equals(that: Any) = that match {
      case that: UndirectedGraph.EdgeProxy[K, E] =>
        implicit val K = graph.eqOnKeys
        (this.graph eq that.graph) &&
          ((this.key1 === that.key1) && (this.key2 === that.key2)) ||
          ((this.key1 === that.key2) && (this.key2 === that.key1))
      case _ => false
    }
    override def toString = s"{$key1, $key2}"
    override def hashCode = poly.algebra.Hashing.byRef.hash(graph) + (graph.eqOnKeys match {
      case hk: Hashing[K] => hk.hash(key1) ^ hk.hash(key2)
      case _ => key1.## ^ key2.##
    })
  }
}

abstract class AbstractUndirectedGraph[K, +E] extends AbstractBiGraph[K, E] with UndirectedGraph[K, E]
