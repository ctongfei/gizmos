package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.collection.search._

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents a directed graph in which each node's successors can be efficiently retrieved.
 * @tparam K Type of keys
 * @tparam E Type of data associated with edges
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Graph[@sp(Int) K, @sp(Double) +E] extends EqStateSpace[K] with KeyedLike[K, Graph[K, E]] with PartialFunction[(K, K), E] { self =>

  import Graph._

  /** Returns the set of the keys of the vertices in this graph. */
  def keySet: Set[K]

  /** Returns the equivalence relation on the keys of this graph. */
  implicit def keyEq = keySet.keyEq

  /** Gets the data on the arc indexed by the specific two keys. */
  def apply(i: K, j: K): E

  /** Gets the data on the arc indexed by the specific two keys. */
  def apply(ij: (K, K)) = apply(ij._1, ij._2)

  /** Optionally retrieves the data on the edge indexed by the specific two keys. */
  def ?(i: K, j: K): Option[E]

  /** Returns an iterable collection of the keys in this graph. */
  def keys = keySet.keys

  /** Returns whether a key is present in this graph. */
  def containsKey(i: K): Boolean = keySet.contains(i)

  /** Returns whether an arc is present between the two given keys in this graph. */
  def containsArc(i: K, j: K): Boolean

  final def notContainsArc(i: K, j: K) = !containsArc(i, j)

  /** Returns the outgoing set of keys of a given key. */
  def outgoingKeySet(i: K): Set[K]

  /** Gets the node with the specific key. */
  def node(i: K): GraphNode[K, E] = new NodeProxy(self, i)

  /** Returns the number of node in this graph. */
  def numNodes: Int = keys.size

  /** Returns an iterable collection of the node in this graph. */
  def nodes: Iterable[GraphNode[K, E]] = keys map node

  // ARCS
  def isDefinedAt(x: (K, K)) = containsArc(x._1, x._2)

  /** Gets the edge between the specific vertices. */
  def arc(i: K, j: K) = (i, j, self(i, j))

  /** Returns the number of edges in this graph. */
  def numArcs: Int = arcs.size

  def arcs: Iterable[(K, K, E)] = for (i <- keys; (j, e) <- outgoingMap(i).pairs) yield (i, j, e)
  // for (i <- keys; j: K <- outgoingKeySet(i)) yield (i, j, arc(i, j)) failed: SI-9963

  def arcMap: Map[(K, K), E] = (keySet createMap outgoingMap).uncurry

  def containsNode(i: K) = keySet.contains(i)

  def outgoingMap(i: K) = outgoingKeySet(i) createMap { j => apply(i, j) }
  def outgoingKeys(i: K) = outgoingKeySet(i).elements
  def outgoingNodes(i: K) = outgoingKeys(i) map node
  def outgoingArcs(i: K) = outgoingKeys(i) map { j => arc(i, j) }

  def outDegree(i: K) = outgoingKeySet(i).size

  def succ(i: K) = outgoingKeys(i)

  /** @inheritdoc In this case, the relation is the adjacency relation on vertices. */
  override def related(i: K, j: K) = containsArc(i, j)

  // HELPER FUNCTIONS

  /** Transforms the data on all the arcs of this graph. */
  def map[F](f: E => F): Graph[K, F] = new GraphT.Mapped(self, f)

  def mapWithKeys[F](f: (K, K, E) => F): Graph[K, F] = new GraphT.MappedWithKeys(self, f)

  override def filterKeys(f: K => Boolean): Graph[K, E] = new GraphT.KeyFiltered(self, f)

  def zip[F](that: Graph[K, F]): Graph[K, (E, F)] = zipWith(that)((e, f) => (e, f))

  def zipWith[F, X](that: Graph[K, F])(f: (E, F) => X): Graph[K, X] = new GraphT.ZippedWith(self, that, f)

  /** Wraps the keys of this graph by a bijective function. */
  def contramap[J](f: Bijection[J, K]): Graph[J, E] = new GraphT.Contramapped(self, f)

  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BidiGraph[K, E] = AdjacencyListBidiGraph fromEdges (arcs map { case (i, j, e) => (j, i, e) })

  /** Casts this graph as a multimap that maps a key to the outgoing set of that key. */
  def asMultimap: Multimap[K, K] = new GraphT.AsMultimap(self)

  def to[G[_, _], Ev[_], F >: E](factory: FactoryAAB_EvA[G, Ev])(implicit K: Ev[K]): G[K, F] = factory from arcs

  override def toString = "{" + arcs.map(e => s"${e._1} –(${e._3})-> ${e._2}").buildString(", ") + "}"
}

object Graph {

  class NodeProxy[K, +E](val graph: Graph[K, E], val key: K) extends GraphNode[K, E] {
    def outgoingMap = graph.outgoingMap(key)
    def succ = graph.succ(key) map { i => new NodeProxy(graph, i) }
    def isDummy = graph notContainsKey key
    def outgoingKeySet = graph.outgoingKeySet(key)

    // special implementation: uses the eqOnKeys in the referring graph!
    // ensures that two NodeProxy objects are really referring to the same node in the graph
    override def equals(that: Any) = that match {
      case that: NodeProxy[K, E] => (this.graph eq that.graph) && graph.keyEq.eq(this.key, that.key)
      case _ => false
    }
    override def hashCode = hashByRef(graph) + (graph.keyEq match {
      case hk: Hashing[K] => hk.hash(key)
      case _ => key.##
    })

  }

  implicit class AsWeightedStateSpace[K, E: OrderedAdditiveGroup](g: Graph[K, E]) extends WeightedStateSpace[K, E] {
    def groupOnCost = OrderedAdditiveGroup[E]
    def succWithCost(x: K) = g.outgoingMap(x).pairs
    def keyEq = g.keyEq
  }

}

abstract class AbstractGraph[@sp(Int) K, +E] extends Graph[K, E]

private[poly] object GraphT {

  class Mapped[K, E, F](self: Graph[K, E], f: E => F) extends AbstractGraph[K, F] {
    def apply(i: K, j: K) = f(self(i, j))
    def ?(i: K, j: K) = (self ? (i, j)) map f
    def keySet = self.keySet
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def outgoingKeySet(i: K) = self.outgoingKeySet(i)
  }

  class MappedWithKeys[K, E, F](self: Graph[K, E], f: (K, K, E) => F) extends AbstractGraph[K, F] {
    def keySet = self.keySet
    def apply(i: K, j: K) = f(i, j, self(i, j))
    def ?(i: K, j: K) = if (self.containsArc(i, j)) Some(f(i, j, self(i, j))) else None
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def outgoingKeySet(i: K) = self.outgoingKeySet(i)
  }

  class KeyFiltered[K, E](self: Graph[K, E], f: K => Boolean) extends AbstractGraph[K, E] {
    def keySet = self.keySet filter f
    def apply(i: K, j: K) = self(i, j)
    def ?(i: K, j: K) = if (f(i) && f(j)) self ? (i, j) else None
    def containsArc(i: K, j: K) = self.containsArc(i, j) && f(i) && f(j)
    def outgoingKeySet(i: K) = self.outgoingKeySet(i) filterKeys f
  }

  class ZippedWith[K, E, F, X](self: Graph[K, E], that: Graph[K, F], f: (E, F) => X) extends AbstractGraph[K, X] {
    def keySet = self.keySet intersect that.keySet
    def apply(i: K, j: K) = f(self(i, j), that(i, j))
    def ?(i: K, j: K) = for (a <- self ? (i, j); b <- that ? (i, j)) yield f(a, b)
    def containsArc(i: K, j: K) = self.containsArc(i, j) && that.containsArc(i, j)
    def outgoingKeySet(i: K) = self.outgoingKeySet(i) intersect that.outgoingKeySet(i)
  }

  class Contramapped[K, E, J](self: Graph[K, E], f: Bijection[J, K]) extends AbstractGraph[J, E] {
    def keySet = self.keySet contramap f
    def apply(i: J, j: J) = self(f(i), f(j))
    def ?(i: J, j: J) = self ? (f(i), f(j))
    def containsArc(i: J, j: J) = self.containsArc(f(i), f(j))
    def outgoingKeySet(i: J) = self.outgoingKeySet(f(i)) contramap f
  }

  class AsMultimap[K](self: Graph[K, Any]) extends AbstractMultimap[K, K] {
    def keySet = self.keySet
    def valueEq = self.keyEq
    def apply(k: K) = self.outgoingKeySet(k)
  }

}
