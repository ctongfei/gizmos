package poly.collection.mut

import poly.algebra._

/**
 * @author Tongfei Chen
 */
trait PriorityQueue[T] extends Queue[T] {

  /** Retrieves the order under which the elements are sorted. */
  def order: WeakOrder[T]

}
