package poly.collection.graph

import poly.collection._
import poly.collection.node._
import poly.util.specgroup._

/**
 * Basic trait for a forward directed graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Graph[@sp(i) I, +V, +E] { self =>

  def apply(i: I): V
  def apply(i: I, j: I): E

  def vertex(i: I): Vertex = new Vertex(i)
  def edge(i: I, j: I): Edge = Edge(i, j)

  def numVertices: Int = ids.size
  def numEdges: Int = edges.size

  def ids: Set[I]
  def vertices: Set[Vertex]
  def edges: Set[Edge]

  def containsVertex(i: I): Boolean
  def containsEdge(i: I, j: I): Boolean

  def outgoingIdsOf(i: I): Enumerable[I]
  def outgoingVerticesOf(i: I): Enumerable[Vertex] = outgoingIdsOf(i).map(j => new Vertex(j))
  def outgoingEdgesOf(i: I): Enumerable[Edge] = outgoingIdsOf(i).map(j => Edge(i, j))
  def outDegree(i: I) = outgoingIdsOf(i).size

  class Vertex(val id: I) extends Node[V] {
    def data = self.apply(id)
    def succ = self.outgoingVerticesOf(id)
  }

  case class Edge(id1: I, id2: I) {
    def data = self.apply(id1, id2)
  }

}
