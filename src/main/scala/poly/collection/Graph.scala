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
 * @tparam V Type of data associated with vertices
 * @tparam E Type of data associated with edges
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Graph[@sp(Int) K, +V, +E] extends KeyedLike[K, Graph[K, V, E]] with StateSpace[K] { self =>

  import Graph._

  /** Gets the data on the node indexed by the specific key. */
  def apply(i: K): V

  /** Gets the data on the edge indexed by the specific two keys. */
  def apply(i: K, j: K): E

  /** Gets the node with the specific key. */
  def node(i: K) = new Node(self, i)

  /** Gets the edge between the specific vertices. */
  def arc(i: K, j: K) = new Arc(self, i, j)

  /** Returns the number of vertices in this graph. */
  def numNodes: Int = keySet.size

  /** Returns the number of edges in this graph. */
  def numArcs: Int = arcs.size

  implicit def eqOnKeys = keySet.eqOnKeys

  /** Returns the set of the keys of the vertices in this graph. */
  def keySet: Set[K]

  /** Returns an iterable collection of the keys in this graph. */
  def keys: Iterable[K] = keySet.elements

  /** Returns a map that maps keys to the data on corresponding vertices. */
  def nodeMap: Map[K, V] = keySet createMapBy apply

  /** Returns an iterable collection of the vertices in this graph. */
  def nodes: Iterable[Node[K, V]] = keys.map(node)

  def arcMap: Map[(K, K), E] = (keySet createMapBy outgoingMapOf).uncurry

  def arcs: Iterable[Arc[K, E]] = for (i ← keys; j ← outgoingKeysOf(i)) yield arc(i, j)

  def containsKey(i: K) = keySet.contains(i)
  def containsNode(i: K) = keySet.contains(i)
  def containsArc(i: K, j: K) = outgoingMapOf(i).containsKey(j)

  def outgoingMapOf(i: K): Map[K, E]

  def outgoingKeysOf(i: K) = outgoingMapOf(i).keys
  def outgoingNodesOf(i: K) = outgoingKeysOf(i).map(j => node(j))
  def outgoingArcsOf(i: K) = outgoingKeysOf(i).map(j => arc(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  def succ(i: K) = outgoingKeysOf(i)

  def adjacent(i: K, j: K) = containsArc(i, j)

  // HELPER FUNCTIONS

  def mapNodes[W](f: V => W): Graph[K, W, E] = new AbstractGraph[K, W, E] {
    def apply(i: K) = f(self(i))
    def keySet = self.keySet
    def apply(i: K, j: K) = self.apply(i, j)
    def outgoingMapOf(i: K) = self.outgoingMapOf(i)
  }

  def mapArcs[F](f: E => F): Graph[K, V, F] = new AbstractGraph[K, V, F] {
    def apply(i: K) = self(i)
    def keySet = self.keySet
    def apply(i: K, j: K) = f(self.apply(i, j))
    def outgoingMapOf(i: K) = self.outgoingMapOf(i).map(f)
  }

  /**
   * Returns the subgraph with only the nodes selected by the given predicate.
   * @param f Node selector
   * @return A subgraph with only the nodes selected. An edge will be selected iff both its ends are selected
   *         by the predicate.
   */
  override def filterKeys(f: K => Boolean): Graph[K, V, E] = new AbstractGraph[K, V, E] {
    def apply(i: K) = self(i)
    override def containsArc(i: K, j: K) = self.containsArc(i, j) && f(i) && f(j)
    def apply(i: K, j: K) = self(i, j)
    def outgoingMapOf(i: K) = if (f(i)) self.outgoingMapOf(i).filterKeys(f) else Map.empty[K]
    def keySet = self.keySet.filterKeys(f)
  }


  def zip[W, F](that: Graph[K, W, F]): Graph[K, (V, W), (E, F)] = new AbstractGraph[K, (V, W), (E, F)] {
    def apply(i: K) = (self(i), that(i))
    override def containsArc(i: K, j: K) = self.containsArc(i, j) && that.containsArc(i, j)
    def apply(i: K, j: K) = (self(i, j), that(i, j))
    def outgoingMapOf(i: K) = self.outgoingMapOf(i) zip that.outgoingMapOf(i)
    def keySet = self.keySet & that.keySet
  }

  def contramap[J](f: J <=> K): Graph[J, V, E] = new AbstractGraph[J, V, E] {
    def apply(i: J) = self.apply(f(i))
    override def containsArc(i: J, j: J) = self.containsArc(f(i), f(j))
    def apply(i: J, j: J) = self.apply(f(i), f(j))
    def outgoingMapOf(i: J) = self.outgoingMapOf(f(i)) contramap f
    def keySet = self.keySet contramap f
  }

  /**
   * Returns the reverse/transpose graph of the original graph.
   * @return The reverse graph, in which every edge is reversed
   */
  def reverse: BiGraph[K, V, E] = {
    ???
  }

  def to[G[_, _, _], W >: V, F >: E](factory: GraphFactory[G]): G[K, W, F] = {
    val b = factory.newBuilder[K, W, F]
    b.numNodesHint(self.numNodes)
    b.addNodes(self.nodes.map(v => (v.key, v.data)))
    b.addEdges(self.arcs.map(e => (e.source, e.target, e.data)))
    b.result
  }

}

object Graph {
  class Node[K, +V](val graph: Graph[K, V, _], val key: K) extends ForwardNodeLike[V, Node[K, V]] {
    def isDummy = false
    def data = graph(key)
    def succ = graph.outgoingNodesOf(key)

    implicit def equivOnKey = graph.eqOnKeys

    override def equals(that: Any) = that match {
      case that: Node[K, V] => (this.graph eq that.graph) && (this.key === that.key)
    }
    override def hashCode = graph.## + key.## //TODO: ???
  }


  class Arc[K, +E](val graph: Graph[K, _, E], val source: K, val target: K) {

    def data = graph.apply(source, target)

    implicit def equivOnKey = graph.eqOnKeys

    override def equals(that: Any) = that match {
      case that: Arc[K, E] => (this.graph eq that.graph) && (this.source === that.source) && (this.target === that.target)
    }
    //TODO: hashing
  }

  implicit class AsWeightedStateSpace[K, E](g: Graph[K, _, E])(implicit E: OrderedAdditiveGroup[E]) extends WeightedStateSpace[K, E] {
    implicit def groupOnCost = E
    def succWithCost(x: K) = g.outgoingArcsOf(x).map(e => (e.target, e.data))

    def eqOnKeys = g.eqOnKeys
  }

}

abstract class AbstractGraph[@sp(Int) K, +V, +E] extends AbstractStateSpace[K] with Graph[K, V, E]
