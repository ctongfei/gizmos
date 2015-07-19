package poly.collection.graph

import scala.math.{min, max}

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Edge[+E] {
  def id1: Int
  def id2: Int
  def data: E
}

case class DirectedEdge[+E](id1: Int, id2: Int, data: E) extends Edge[E] {
  def source = id1
  def target = id2
}

case class UndirectedEdge[+E](id1: Int, id2: Int, data: E) extends Edge[E] {

  override def equals(that: Any) = that match {
    case that: UndirectedEdge[E] =>
      (this.data == that.data) && (
        (this.id1 == that.id1 && this.id2 == that.id2) ||
          (this.id1 == that.id2 && this.id2 == that.id1)
        )
    case _ => false
  }

  override def hashCode = {
    if (id1 <= id2) DirectedEdge(id1, id2, data).##
    else DirectedEdge(id2, id1, data).##
  }

  override def toString = s"{${min(id1, id2)}, ${max(id1, id2)}} [$data]"

}