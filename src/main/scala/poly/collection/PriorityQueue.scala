package poly.collection

import poly.algebra._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait PriorityQueue[T] extends Queue[T] {

  /** Retrieves the order under which the elements are sorted. */
  def order: WeakOrder[T]

}
