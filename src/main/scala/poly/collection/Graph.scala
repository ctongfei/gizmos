package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.node._
import poly.collection.search._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents a forward directed graph.
 *
 * @tparam K Type of keys
 * @tparam V Type of data associated with vertices
 * @tparam E Type of data associated with edges
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Graph[@sp(i) K, +V, +E] extends KeyedStructure[K, Graph[K, V, E]] with StateSpace[K] { self =>

  import Graph._

  /** Gets the data on the node indexed by the specific key. */
  def apply(i: K): V

  /** Gets the data on the edge indexed by the specific two keys. */
  def apply(i: K, j: K): E

  /** Gets the node with the specific key. */
  def node(i: K) = new Node(self, i)

  /** Gets the edge between the specific vertices. */
  def edge(i: K, j: K) = new Edge(self, i, j)

  /** Returns the number of vertices in this graph. */
  def numNodes: Int = keySet.size

  /** Returns the number of edges in this graph. */
  def numEdges: Int = edges.size

  implicit def equivOnState = keySet.equivOnKey

  /** Returns the set of the keys of the vertices in this graph. */
  def keySet: Set[K]

  /** Returns an iterable collection of the keys in this graph. */
  def keys: Iterable[K] = keySet.elements

  /** Returns a map that maps keys to the data on corresponding vertices. */
  def nodeMap: Map[K, V] = keySet createMapBy (k => self(k))

  /** Returns an iterable collection of the vertices in this graph. */
  def nodes: Iterable[Node[K, V]] = keys.map(node)
  def edgeMap: Map[(K, K), E] = ???
  def edges: Iterable[Edge[K, E]] = for (i ← keys; j ← outgoingKeysOf(i)) yield edge(i, j)

  final def containsKey(i: K) = containsNode(i)
  def containsNode(i: K): Boolean
  def containsEdge(i: K, j: K): Boolean

  def outgoingKeysOf(i: K): Iterable[K]
  def outgoingNodesOf(i: K) = outgoingKeysOf(i).map(j => node(j))
  def outgoingEdgesOf(i: K) = outgoingKeysOf(i).map(j => edge(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  def succ(i: K) = outgoingKeysOf(i)

  // HELPER FUNCTIONS

  def mapNodes[V1](f: V => V1): Graph[K, V1, E] = new AbstractGraph[K, V1, E] {
    def apply(i: K): V1 = f(self(i))
    def containsEdge(i: K, j: K): Boolean = self.containsEdge(i, j)
    def containsNode(i: K): Boolean = self.containsNode(i)
    def keySet: Set[K] = self.keySet
    def apply(i: K, j: K): E = self.apply(i, j)
    def outgoingKeysOf(i: K): Iterable[K] = self.outgoingKeysOf(i)
  }

  def mapEdges[E1](f: E => E1): Graph[K, V, E1] = new AbstractGraph[K, V, E1] {
    def apply(i: K): V = self(i)
    def containsEdge(i: K, j: K): Boolean = self.containsEdge(i, j)
    def containsNode(i: K): Boolean = self.containsNode(i)
    def keySet: Set[K] = self.keySet
    def apply(i: K, j: K): E1 = f(self.apply(i, j))
    def outgoingKeysOf(i: K): Iterable[K] = self.outgoingKeysOf(i)
  }

  /**
    * Returns the subgraph with only the nodes selected by the given predicate.
    * @param f Node selector
    * @return A subgraph with only the nodes selected. An edge will be selected iff both its ends are selected
    *         by the predicate.
    */
  def filterKeys(f: K => Boolean): Graph[K, V, E] = ???

  def filterNodes(f: V => Boolean): Graph[K, V, E] = new AbstractGraph[K, V, E] {
    def apply(i: K): V = {
      if (f(self(i))) self(i)
      else throw new KeyNotFoundException(i)
    }
    def containsEdge(i: K, j: K): Boolean = f(self(i)) && f(self(j))
    def containsNode(i: K): Boolean = f(self(i))
    def apply(i: K, j: K): E = if (containsEdge(i, j)) self(i, j) else throw new KeyNotFoundException(i, j)
    def outgoingKeysOf(i: K): Iterable[K] = ???
    def keySet: Set[K] = ???
  }

  def zip[V1, E1](that: Graph[K, V1, E1]): Graph[K, (V, V1), (E, E1)] = new AbstractGraph[K, (V, V1), (E, E1)] {
    def apply(i: K) = (self(i), that(i))
    def containsNode(i: K) = self.containsNode(i) && that.containsNode(i)
    def containsEdge(i: K, j: K) = self.containsEdge(i, j) && that.containsEdge(i, j)
    def apply(i: K, j: K) = (self(i, j), that(i, j))
    def outgoingKeysOf(i: K) = ??? // self.outgoingKeysOf(i) intersect that.outgoingKeysOf(i)
    def keySet = self.keySet & that.keySet
  }

  def to[G[_, _, _]](implicit builder: GraphBuilder[K, V@uv, E@uv, G[K, V@uv, E@uv]]): G[K, V@uv, E@uv] = {
    val b = builder
    b.numNodesHint(self.numNodes)
    b.addNodes(self.nodes.map(v => (v.key, v.data)))
    b.addEdges(self.edges.map(e => (e.key1, e.key2, e.data)))
    b.result
  }

}

object Graph {
  class Node[K: Equiv, +V](val graph: Graph[K, V, _], val key: K) extends ForwardNode[V] {
    def isDummy = false
    def data = graph.apply(key)
    def succ = graph.outgoingNodesOf(key)

    override def equals(that: Any) = that match {
      case that: Node[K, V] => (this.graph eq that.graph) && (this.key =~= that.key)
    }
    //TODO: hashing
  }


  class Edge[K: Equiv, +E](val graph: Graph[K, _, E], val key1: K, val key2: K) {
    def data = graph.apply(key1, key2)

    override def equals(that: Any) = that match {
      case that: Edge[K, E] => (this.graph eq that.graph) && (this.key1 =~= that.key1) && (this.key2 =~= that.key2)
    }
    //TODO: hashing
  }

  implicit class asWeightedStateSpace[K, E](g: Graph[K, _, E])(implicit E: OrderedAdditiveGroup[E]) extends WeightedStateSpace[K, E] {
    implicit def groupOnCost = E
    def succWithCost(x: K) = g.outgoingEdgesOf(x).map(e => (e.key2, e.data))
    def equivOnState = g.equivOnState
  }

}

abstract class AbstractGraph[@sp(i) K, +V, +E] extends Graph[K, V, E]
