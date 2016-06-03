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
 *
 * @tparam K Type of keys
 * @tparam E Type of data associated with edges
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Graph[@sp(Int) K, +E] extends KeyedLike[K, Graph[K, E]] with StateSpace[K] { self =>

  import Graph._

  /** Returns the equivalence relation on the keys of this graph. */
  implicit def eqOnKeys: Eq[K]

  /** Gets the data on the edge indexed by the specific two keys. */
  def apply(i: K, j: K): E

  def ?(i: K, j: K): Option[E]

  /** Returns an iterable collection of the keys in this graph. */
  def keys: Iterable[K]

  /** Returns whether a key is present in this graph. */
  def containsKey(i: K): Boolean

  /** Returns whether an arc is present between the two given keys in this graph. */
  def containsArc(i: K, j: K): Boolean

  final def notContainsArc(i: K, j: K) = !containsArc(i, j)

  /** Returns the outgoing set of keys of a given key. */
  def outgoingKeySet(i: K): Set[K]

  // NODES
  /** Gets the node with the specific key. */
  def node(i: K): GraphNode[K, E] = new NodeProxy(self, i)

  /** Returns the number of node in this graph. */
  def numNodes: Int = keys.size

  /** Returns an iterable collection of the node in this graph. */
  def nodes: Iterable[GraphNode[K, E]] = keys map node

  // ARCS
  /** Gets the edge between the specific vertices. */
  def arc(i: K, j: K): GraphArc[K, E] = new ArcProxy(self, i, j)

  /** Returns the number of edges in this graph. */
  def numArcs: Int = arcs.size

  //TODO: compiler bug here: writing j <- outgoingKeySet(i) results in NPE in compiler phase patmat
  def arcs: Iterable[GraphArc[K, E]] = for (i <- keys; j <- outgoingKeys(i)) yield arc(i, j)

  def arcMap: Map[(K, K), E] = (keySet createMapBy outgoingMap).uncurry

  /** Returns the set of the keys of the vertices in this graph. */
  def keySet: Set[K] = new AbstractSet[K] {
    def keys = self.keys
    def contains(k: K) = self.containsKey(k)
    implicit def eqOnKeys = self.eqOnKeys
  }

  def containsNode(i: K) = keySet.contains(i)

  def outgoingMap(i: K) = outgoingKeySet(i) createMapBy { j => apply(i, j) }
  def outgoingKeys(i: K) = outgoingKeySet(i).elements
  def outgoingNodes(i: K) = outgoingKeys(i) map node
  def outgoingArcs(i: K) = outgoingKeys(i) map { j => arc(i, j) }

  def outDegree(i: K) = outgoingKeySet(i).size

  def succ(i: K) = outgoingKeys(i)

  // HELPER FUNCTIONS

  def map[F](f: E => F): Graph[K, F] = new GraphT.Mapped(self, f)

  def mapWithKeys[F](f: GraphArc[K, E] => F): Graph[K, F] = new GraphT.MappedWithKeys(self, f)

  override def filterKeys(f: K => Boolean): Graph[K, E] = new GraphT.KeyFiltered(self, f)

  def zip[F](that: Graph[K, F]): Graph[K, (E, F)] = zipWith(that)((e, f) => (e, f))

  def zipWith[F, X](that: Graph[K, F])(f: (E, F) => X): Graph[K, X] = new GraphT.ZippedWith(self, that, f)

  def contramap[J](f: Bijection[J, K]): Graph[J, E] = new GraphT.Contramapped(self, f)

  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BiGraph[K, E] = AdjacencyListBiGraph from arcs.map(e => (e.target, e.source, e.data))

  def to[G[_, _], Ev[_], F >: E](factory: FactoryAAeB[G, Ev])(implicit K: Ev[K]): G[K, F] = factory from arcs.map(e => (e.source, e.target, e.data: F))


  override def toString = "{" + arcs.map(e => s"${e.target} –(${e.data})-> ${e.data}").buildString(", ") + "}"
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
      case that: NodeProxy[K, E] => (this.graph eq that.graph) && graph.eqOnKeys.eq(this.key, that.key)
      case _ => false
    }
    override def hashCode = poly.algebra.Hashing.byRef.hash(graph) + (graph.eqOnKeys match {
      case hk: Hashing[K] => hk.hash(key)
      case _ => key.##
    })

  }

  class ArcProxy[K, +E](val graph: Graph[K, E], val source: K, val target: K) extends GraphArc[K, E] {

    def data = graph.apply(source, target)

    def sourceNode = graph.node(source)
    def targetNode = graph.node(target)

    override def equals(that: Any) = that match {
      case that: ArcProxy[K, E] => (this.graph eq that.graph) && (this.source == that.source) && (this.target == that.target)
      case _ => false
    }

    override def hashCode = graph.## + (source, target).##
  }

  implicit class AsWeightedStateSpace[K, E: OrderedAdditiveGroup](g: Graph[K, E]) extends WeightedStateSpace[K, E] {
    def groupOnCost = OrderedAdditiveGroup[E]
    def succWithCost(x: K) = g.outgoingArcs(x).map(e => (e.target, e.data))
    def eqOnKeys = g.eqOnKeys
  }

}

abstract class AbstractGraph[@sp(Int) K, +E] extends Graph[K, E]

private[poly] object GraphT {

  class Mapped[K, E, F](self: Graph[K, E], f: E => F) extends AbstractGraph[K, F] {
    def apply(i: K, j: K) = f(self(i, j))
    def ?(i: K, j: K) = (self ? (i, j)) map f
    def keys = self.keys
    def containsKey(i: K) = self.containsKey(i)
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def eqOnKeys = self.eqOnKeys
    def outgoingKeySet(i: K) = self.outgoingKeySet(i)
  }

  class MappedWithKeys[K, E, F](self: Graph[K, E], f: GraphArc[K, E] => F) extends AbstractGraph[K, F] {
    def apply(i: K, j: K) = f(self.arc(i, j))
    def ?(i: K, j: K) = if (self.containsArc(i, j)) Some(f(self.arc(i, j))) else None
    def keys = self.keys
    def containsKey(i: K) = self.containsKey(i)
    def containsArc(i: K, j: K) = self.containsArc(i, j)
    def eqOnKeys = self.eqOnKeys
    def outgoingKeySet(i: K) = self.outgoingKeySet(i)
  }

  class KeyFiltered[K, E](self: Graph[K, E], f: K => Boolean) extends AbstractGraph[K, E] {
    def apply(i: K, j: K) = self(i, j)
    def ?(i: K, j: K) = if (f(i) && f(j)) self ? (i, j) else None
    def keys = self.keys filter f
    def containsKey(i: K) = self.containsKey(i) && f(i)
    def containsArc(i: K, j: K) = self.containsArc(i, j) && f(i) && f(j)
    def eqOnKeys = self.eqOnKeys
    def outgoingKeySet(i: K) = self.outgoingKeySet(i) filterKeys f
  }

  class ZippedWith[K, E, F, X](self: Graph[K, E], that: Graph[K, F], f: (E, F) => X) extends AbstractGraph[K, X] {
    def apply(i: K, j: K) = f(self(i, j), that(i, j))
    def ?(i: K, j: K) = for (a <- self ? (i, j); b <- that ? (i, j)) yield f(a, b)
    def keys = (self.keySet intersect that.keySet).keys
    def containsKey(i: K) = self.containsKey(i) && that.containsKey(i)
    def containsArc(i: K, j: K) = self.containsArc(i, j) && that.containsArc(i, j)
    def eqOnKeys = self.eqOnKeys
    def outgoingKeySet(i: K) = self.outgoingKeySet(i) intersect that.outgoingKeySet(i)
  }

  class Contramapped[K, E, J](self: Graph[K, E], f: Bijection[J, K]) extends AbstractGraph[J, E] {
    def apply(i: J, j: J) = self(f(i), f(j))
    def ?(i: J, j: J) = self ? (f(i), f(j))
    def keys = self.keys map f.invert
    def containsKey(i: J) = self.containsKey(f(i))
    def containsArc(i: J, j: J) = self.containsArc(f(i), f(j))
    def eqOnKeys = self.eqOnKeys contramap f
    def outgoingKeySet(i: J) = self.outgoingKeySet(f(i)) contramap f
  }

}
