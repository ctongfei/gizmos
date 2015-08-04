package poly.collection

import poly.collection.node._
import poly.util.specgroup._

/**
 * Basic trait for a forward directed graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Graph[@sp(i) K, +V, +E] { self =>

  def apply(i: K): V
  def apply(i: K, j: K): E

  def vertex(i: K): Vertex = new Vertex(i)
  def edge(i: K, j: K): Edge = Edge(i, j)

  def numVertices: Int = keys.size
  def numEdges: Int = edges.size

  def keys: Set[K]
  def vertices: Set[Vertex]
  def edges: Set[Edge]

  def containsVertex(i: K): Boolean
  def containsEdge(i: K, j: K): Boolean

  def outgoingKeysOf(i: K): Enumerable[K]
  def outgoingVerticesOf(i: K): Enumerable[Vertex] = outgoingKeysOf(i).map(j => new Vertex(j))
  def outgoingEdgesOf(i: K): Enumerable[Edge] = outgoingKeysOf(i).map(j => Edge(i, j))
  def outDegree(i: K) = outgoingKeysOf(i).size

  class Vertex(val id: K) extends Node[V] {
    def data = self.apply(id)
    def succ = self.outgoingVerticesOf(id)
  }

  case class Edge(id1: K, id2: K) {
    def data = self.apply(id1, id2)
  }

}
