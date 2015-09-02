package poly.collection

import poly.collection.mut._

/**
 * Represents an undirected graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait UndirectedGraph[K, +V, +E] extends BiGraph[K, V, E] { self =>

  def incidentKeysOf(i: K): Iterable[K]
  def incidentVerticesOf(i: K): Iterable[Vertex] = incidentKeysOf(i).map(j => new Vertex(j))
  def incidentEdgesOf(i: K): Iterable[UndirectedEdge] = incidentKeysOf(i).map(j => new UndirectedEdge(i, j))

  def outgoingKeysOf(v: K): Iterable[K] = incidentKeysOf(v)
  override def outgoingVerticesOf(v: K): Iterable[Vertex] = incidentVerticesOf(v)
  override def outgoingEdgesOf(v: K): Iterable[UndirectedEdge] = incidentEdgesOf(v)

  def incomingKeysOf(v: K): Iterable[K] = incidentKeysOf(v)
  override def incomingVerticesOf(v: K): Iterable[Vertex] = incidentVerticesOf(v)
  override def incomingEdgesOf(v: K): Iterable[UndirectedEdge] = incidentEdgesOf(v)

  override def reverse = self

  override def edges = ??? //TODO: super.edges.distinct

  class UndirectedEdge(override val key1: K, override val key2: K) extends super.Edge(key1, key2) with Set[K] {
    override def equals(that: Any) = that match {
      case that: UndirectedEdge =>
        (this.key1 == that.key1 && this.key2 == that.key2) ||
        (this.key1 == that.key2 && this.key2 == that.key1)
      case _ => false
    }

    def size = 2
    override def hashCode = key1.## ^ key2.##
    def contains(x: K) = x == key1 || x == key2
    def elements = ListSeq(key1, key2)
  }

}
