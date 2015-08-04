package poly.collection

import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait UndirectedGraph[K, +V, +E] extends BiGraph[K, V, E] {

  def incidentIdsOf(i: K): Enumerable[K]
  def incidentVerticesOf(i: K): Enumerable[Vertex] = incidentIdsOf(i).map(j => new Vertex(j))
  def incidentEdgesOf(i: K): Enumerable[UndirectedEdge] = incidentIdsOf(i).map(j => new UndirectedEdge(i, j))

  def outgoingKeysOf(v: K): Enumerable[K] = incidentIdsOf(v)
  override def outgoingVerticesOf(v: K): Enumerable[Vertex] = incidentVerticesOf(v)
  override def outgoingEdgesOf(v: K): Enumerable[UndirectedEdge] = incidentEdgesOf(v)

  def incomingIdsOf(v: K): Enumerable[K] = incidentIdsOf(v)
  override def incomingVerticesOf(v: K): Enumerable[Vertex] = incidentVerticesOf(v)
  override def incomingEdgesOf(v: K): Enumerable[UndirectedEdge] = incidentEdgesOf(v)


  class UndirectedEdge(override val id1: K, override val id2: K) extends super.Edge(id1, id2) with Set[K] {
    override def equals(that: Any) = that match {
      case that: UndirectedEdge =>
        (this.id1 == that.id1 && this.id2 == that.id2) ||
        (this.id1 == that.id2 && this.id2 == that.id1)
      case _ => false
    }

    def size = 2
    override def hashCode = id1.## ^ id2.##
    def contains(x: K) = x == id1 || x == id2
    def elements = ListSeq(id1, id2)
  }

}
