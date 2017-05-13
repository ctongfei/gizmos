package poly.collection.mut

import cats.implicits._
import poly.collection._
import scala.language.higherKinds

/**
  * Represents a beam, which is a priority queue that only keeps the ''k'' smallest elements.
  * @author Tongfei Chen
  * @since 0.1.0
 */
class Beam[T] private(private val capacity: Int, private val pq: PriorityQueue[T]) extends Queue[T] {

  implicit def elementOrder = pq.elementOrder.reverse

  override def size = pq.size

  def enqueue(x: T) = {
    if (size < capacity)
      pq enqueue x
    else if (x < pq.front) {
      pq dequeue()
      pq enqueue x
    }
    else { /* discard this element */ }
  }

  def elements = pq.elements

  /** Returns the '''LARGEST''' (not the smallest!) element in the beam. */
  def front = pq.front

  def dequeue() = pq.dequeue()
}

object Beam {

  /**
   * Constructs an empty beam with the specified width ''k''.
   * It keeps the ''k'' smallest elements under the given implicit order.
   */
  def ofSize[T](k: Int)(implicit T: Order[T]): Beam[T] = new Beam(k, BinaryHeap()(T.reverse))

}
