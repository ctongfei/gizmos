package poly.collection

import poly.collection.exception._
import poly.collection.node._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents a forward directed graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Graph[@sp(i) K, +V, +E] extends Keyed[K] { self =>

  def apply(i: K): V
  def apply(i: K, j: K): E

  /** Gets the node with the specific key. */
  def node(i: K): Node = new Node(i)

  /** Gets the edge between the specific edges. */
  def edge(i: K, j: K): Edge = Edge(i, j)

  def numNodes: Int = keySet.size
  def numEdges: Int = edges.size

  def equivOnKey = keySet.equivOnKey
  def keySet: Set[K]
  def keys: Iterable[K] = keySet.elements
  def nodes: Iterable[Node] = keys.map(i => new Node(i))
  def edges: Iterable[Edge] = for (i ← keys; j ← outgoingKeysOf(i)) yield new Edge(i, j)

  def containsNode(i: K): Boolean
  def containsEdge(i: K, j: K): Boolean

  def outgoingKeysOf(i: K): Iterable[K]
  def outgoingNodesOf(i: K): Iterable[Node] = outgoingKeysOf(i).map(j => new Node(j))
  def outgoingEdgesOf(i: K): Iterable[Edge] = outgoingKeysOf(i).map(j => Edge(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  class Node(val key: K) extends ForwardNode[V] {
    def isDummy = false
    def data = self.apply(key)
    def succ = self.outgoingNodesOf(key)
  }

  case class Edge(key1: K, key2: K) {
    def data = self.apply(key1, key2)
  }

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

abstract class AbstractGraph[@sp(i) K, +V, +E] extends Graph[K, V, E]
