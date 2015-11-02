package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import scala.language.higherKinds

/**
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Beam[T] private(k: Int, val pq: PriorityQueue[T]) extends Iterable[T] with HasKnownSize {

  def capacity = k

  implicit def order = pq.order.reverse

  override def size = pq.size



  def push(x: T) = {
    if (x < pq.top) { // if not, discard
      pq.pop()
      pq.push(x)
    }
  }

  def newIterator = pq.newIterator

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