package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import scala.language.higherKinds

/**
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Beam[T] private(k: Int, val pq: PriorityQueue[T]) extends PriorityQueue[T] {

  def capacity = k

  implicit def order = pq.order

  def top = pq.top

  def pop() = pq.pop()

  def size = pq.size

  def push(x: T) = {
    if (x < top) { // if not, discard
      pq.pop()
      pq.push(x)
    }
  }

}

object Beam {

  /**
   * Constructs an empty beam with the specified width.
   * @param k
   * @param T
   * @tparam T
   * @return
   */
  def ofWidth[T](k: Int)(implicit T: WeakOrder[T]) = new Beam(k, BinaryHeapPriorityQueue()(T.reverse))

}