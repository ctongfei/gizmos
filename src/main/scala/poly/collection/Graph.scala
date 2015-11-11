package poly.collection

import poly.collection.exception._
import poly.collection.node._
import poly.collection.search._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents a forward directed graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Graph[@sp(i) K, +V, +E] extends KeyedStructure[K, Graph[K, V, E]] with StateSpace[K] { self =>

  import Graph._

  def apply(i: K): V
  def apply(i: K, j: K): E

  /** Gets the node with the specific key. */
  def node(i: K) = new Node(self, i)

  /** Gets the edge between the specific edges. */
  def edge(i: K, j: K) = new Edge(self, i, j)

  def numNodes: Int = keySet.size
  def numEdges: Int = edges.size

  def equivOnKey = keySet.equivOnKey
  def keySet: Set[K]
  def keys: Iterable[K] = keySet.elements
  def nodes: Iterable[Graph.Node[K, V]] = keys.map(node)
  def edges: Iterable[Graph.Edge[K, E]] = for (i ← keys; j ← outgoingKeysOf(i)) yield edge(i, j)

  final def containsKey(i: K) = containsNode(i)
  def containsNode(i: K): Boolean
  def containsEdge(i: K, j: K): Boolean

  def outgoingKeysOf(i: K): Iterable[K]
  def outgoingNodesOf(i: K) = outgoingKeysOf(i).map(j => node(j))
  def outgoingEdgesOf(i: K) = outgoingKeysOf(i).map(j => edge(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  def succ(i: K) = outgoingKeysOf(i)


  // HELPER FUNCTIONS

  def mapVertices[V1](f: V => V1): Graph[K, V1, E] = new AbstractGraph[K, V1, E] {
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

  def filterKeys(f: K => Boolean): Graph[K, V, E] = ???

  def filterNodes(f: V => Boolean): Graph[K, V, E] = new AbstractGraph[K, V, E] {
    def apply(i: K): V = {
      if (f(self(i))) self(i)
      else throw new NoSuchElementException
    }
    def containsEdge(i: K, j: K): Boolean = f(self(i)) && f(self(j))
    def containsNode(i: K): Boolean = f(self(i))
    def apply(i: K, j: K): E = if (containsEdge(i, j)) self(i, j) else throw new NoSuchElementException
    def outgoingKeysOf(i: K): Iterable[K] = ???
    def keySet: Set[K] = ???
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
  class Node[K, +V](val graph: Graph[K, V, _], val key: K) extends ForwardNode[V] {
    def isDummy = false
    def data = graph.apply(key)
    def succ = graph.outgoingNodesOf(key)

    override def equals(that: Any) = that match {
      case that: Node[K, V] => (this.graph eq that.graph) && (this.key == that.key)
    }
    //TODO: hashing
  }


  class Edge[K, +E](val graph: Graph[K, _, E], val key1: K, val key2: K) {
    def data = graph.apply(key1, key2)

    override def equals(that: Any) = that match {
      case that: Edge[K, E] => (this.graph eq that.graph) && (this.key1 == that.key1) && (this.key2 == that.key2)
    }
    //TODO: hashing
  }


}

abstract class AbstractGraph[@sp(i) K, +V, +E] extends Graph[K, V, E]
