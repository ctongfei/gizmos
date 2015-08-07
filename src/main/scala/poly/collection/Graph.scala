package poly.collection

import poly.collection.node._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Basic trait for a forward directed graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Graph[@sp(i) K, +V, +E] { self =>

  def apply(i: K): V
  def apply(i: K, j: K): E

  def vertex(i: K): Vertex = new Vertex(i)
  def edge(i: K, j: K): Edge = Edge(i, j)

  def numVertices: Int = keySet.size
  def numEdges: Int = edges.size

  def keySet: Set[K]
  def vertices: Enumerable[Vertex] = keySet.elements.map(i => new Vertex(i))
  def edges: Enumerable[Edge] = for (i ← keySet; j ← outgoingKeysOf(i)) yield new Edge(i, j)

  def containsVertex(i: K): Boolean
  def containsEdge(i: K, j: K): Boolean

  def outgoingKeysOf(i: K): Enumerable[K]
  def outgoingVerticesOf(i: K): Enumerable[Vertex] = outgoingKeysOf(i).map(j => new Vertex(j))
  def outgoingEdgesOf(i: K): Enumerable[Edge] = outgoingKeysOf(i).map(j => Edge(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  class Vertex(val key: K) extends Node[V] {
    def data = self.apply(key)
    def succ = self.outgoingVerticesOf(key)
  }

  case class Edge(key1: K, key2: K) {
    def data = self.apply(key1, key2)
  }

  // HELPER FUNCTIONS

  def mapVertices[V1](f: V => V1): Graph[K, V1, E] = new Graph[K, V1, E] {
    def apply(i: K): V1 = f(self(i))
    def containsEdge(i: K, j: K): Boolean = self.containsEdge(i, j)
    def containsVertex(i: K): Boolean = self.containsVertex(i)
    def keySet: Set[K] = self.keySet
    def apply(i: K, j: K): E = self.apply(i, j)
    def outgoingKeysOf(i: K): Enumerable[K] = self.outgoingKeysOf(i)
  }

  def mapEdges[E1](f: E => E1): Graph[K, V, E1] = new Graph[K, V, E1] {
    def apply(i: K): V = self(i)
    def containsEdge(i: K, j: K): Boolean = self.containsEdge(i, j)
    def containsVertex(i: K): Boolean = self.containsVertex(i)
    def keySet: Set[K] = self.keySet
    def apply(i: K, j: K): E1 = f(self.apply(i, j))
    def outgoingKeysOf(i: K): Enumerable[K] = self.outgoingKeysOf(i)
  }

  def filterVertices(f: (K, V) => Boolean): Graph[K, V, E] = new Graph[K, V, E] {
    def apply(i: K): V = {
      if (f(i, self(i))) self(i)
      else throw new NoSuchElementException
    }
    def containsEdge(i: K, j: K): Boolean = {
      ???
    }

    def containsVertex(i: K): Boolean = ???

    def apply(i: K, j: K): E = ???

    def outgoingKeysOf(i: K): Enumerable[K] = ???

    def keySet: Set[K] = ???
  }

  def to[G[_, _, _]](implicit builder: GraphBuilder[K, V@uv, E@uv, G[K, V@uv, E@uv]]): G[K, V@uv, E@uv] = {
    val b = builder
    b.numVerticesHint(self.numVertices)
    b.addVertices(self.vertices.map(v => (v.key, v.data)))
    b.addEdges(self.edges.map(e => (e.key1, e.key2, e.data)))
    b.result
  }

}
