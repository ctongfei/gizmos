package poly.collection.graph

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait UndirectedGraph[I, +V, +E] extends BiGraph[I, V, E] {

  def incidentIdsOf(i: I): Enumerable[I]
  def incidentVerticesOf(i: I): Enumerable[Vertex] = incidentIdsOf(i).map(j => new Vertex(j))
  def incidentEdgesOf(i: I): Enumerable[UndirectedEdge] = incidentIdsOf(i).map(j => new UndirectedEdge(i, j))

  def outgoingIdsOf(v: I): Enumerable[I] = incidentIdsOf(v)
  override def outgoingVerticesOf(v: I): Enumerable[Vertex] = incidentVerticesOf(v)
  override def outgoingEdgesOf(v: I): Enumerable[UndirectedEdge] = incidentEdgesOf(v)

  def incomingIdsOf(v: I): Enumerable[I] = incidentIdsOf(v)
  override def incomingVerticesOf(v: I): Enumerable[Vertex] = incidentVerticesOf(v)
  override def incomingEdgesOf(v: I): Enumerable[UndirectedEdge] = incidentEdgesOf(v)


  class UndirectedEdge(override val id1: I, override val id2: I) extends super.Edge(id1, id2) with Set[I] {
    override def equals(that: Any) = that match {
      case that: UndirectedEdge =>
        (this.id1 == that.id1 && this.id2 == that.id2) ||
        (this.id1 == that.id2 && this.id2 == that.id1)
      case _ => false
    }

    override def hashCode = id1.## ^ id2.##
    def contains(x: I) = x == id1 || x == id2
    def newEnumerator = ListSeq(id1, id2).newEnumerator
  }


}
