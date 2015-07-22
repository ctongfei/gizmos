package poly.collection.graph

import poly.collection._
import poly.collection.node._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiGraph[@sp(i) I, +V, +E] extends Graph[I, V, E] { self =>

  def incomingIdsOf(i: I): Enumerable[I]
  def incomingVerticesOf(i: I): Enumerable[Vertex] = incomingIdsOf(i).map(j => new Vertex(j))
  def incomingEdgesOf(i: I): Enumerable[Edge] = incomingIdsOf(i).map(j => Edge(j, i))
  def inDegree(i: I) = incomingIdsOf(i).size

  override def outgoingVerticesOf(i: I): Enumerable[Vertex] = outgoingIdsOf(i).map(j => new Vertex(j))

  class Vertex(override val id: I) extends super.Vertex(id) with BiNode[V] {
    def pred = self.incomingVerticesOf(id)
    override def succ = self.outgoingVerticesOf(id)
  }

}
