package poly.collection.graph

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Graph[+V, +E] { self =>

  def apply(i: Int): V
  def apply(i: Int, j: Int): E

  def vertex(i: Int): Vertex[V, E] = Vertex(i, apply(i))(self)
  def edge(i: Int, j: Int): Edge[E]

  def numVertices: Int
  def numEdges: Int

  def ids: Enumerable[Int]
  def vertices: Enumerable[Vertex[V, E]]
  def edges: Enumerable[Edge[E]]

  def containsVertex(i: Int): Boolean
  def containsEdge(i: Int, j: Int): Boolean

  def incomingIdsOf(v: Int): Enumerable[Int]
  def incomingVerticesOf(v: Int): Enumerable[Vertex[V, E]]
  def incomingEdgesOf(v: Int): Enumerable[Edge[E]]

  def outgoingIdsOf(v: Int): Enumerable[Int]
  def outgoingVerticesOf(v: Int): Enumerable[Vertex[V, E]]
  def outgoingEdgesOf(v: Int): Enumerable[Edge[E]]

  def incidentIdsOf(v: Int): Enumerable[Int]
  def incidentVerticesOf(v: Int): Enumerable[Vertex[V, E]]
  def incidentEdgesOf(v: Int): Enumerable[Edge[E]]

  def inDegree(v: Int) = incidentIdsOf(v).size
  def outDegree(v: Int) = outgoingIdsOf(v).size

}

trait DirectedGraph[V, E] extends Graph[V, E] {

  def edge(i: Int, j: Int): DirectedEdge[E] = DirectedEdge(i, j, apply(i, j))

  def incomingEdgesOf(i: Int) = incomingIdsOf(i).map(j => DirectedEdge(j, i, apply(j, i)))
  def outgoingEdgesOf(i: Int) = outgoingIdsOf(i).map(j => DirectedEdge(i, j, apply(i, j)))

}

trait UndirectedGraph[V, E] extends Graph[V, E] {
  def edge(i: Int, j: Int): UndirectedEdge[E] = UndirectedEdge(i, j, apply(i, j))
}