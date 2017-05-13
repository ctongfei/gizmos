package poly.collection.mut

import poly.collection._

/**
 * Represents a mutable priority queue.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait PriorityQueue[T] extends Queue[T] {

  /**
   * Returns an iterable collection of the elements in this priority queue.
   * @note The elements are not guaranteed to be sorted by the order imposed on the priority queue.
   */
  def elements: Iterable[T]

  /** Retrieves the order under which the elements are prioritized. */
  def elementOrder: Order[T]

}
