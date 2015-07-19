package poly.collection.graph

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
case class Vertex[+V, +E](id: Int, data: V)(graph: Graph[V, E]) extends BidiNode[V] {

  def pred = graph.incomingVerticesOf(id)
  def succ = graph.outgoingVerticesOf(id)

}
