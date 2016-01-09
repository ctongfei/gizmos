package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import scala.language.higherKinds

/**
  * Represents a beam, which is a priority queue that only keeps the k-smallest elements.
  * @author Tongfei Chen
  * @since 0.1.0
 */
class Beam[T] private[poly](val capacity: Int, val pq: PriorityQueue[T]) extends Queue[T] with HasKnownSize {

  implicit def order = pq.order.reverse

  override def size = pq.size

  def push(x: T) = {
    if (size < capacity)
      pq push x
    else if (x < pq.top) {
      pq pop()
      pq push x
    }
    else { /* discard this element */ }
  }

  def newIterator = pq.newIterator

  /**
    * @note This is the '''LARGEST''', not the smallest element in the beam.
    * @return the '''LARGEST''' element in the beam
    */
  def top = pq.top

  def pop() = pq.pop()
}

object Beam {

  /**
   * Constructs an empty beam with the specified width.
   */
  def ofWidth[T](k: Int)(implicit T: WeakOrder[T]): Beam[T] = new Beam(k, BinaryHeap()(T.reverse)) {
    override def order = T
  }

}
