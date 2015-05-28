package poly.collection

import poly.algebra._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait PriorityQueue[T] extends Queue[T] {

  /** Retrieves the order under which the elements are sorted. */
  def order: WeakOrder[T]

}

trait PriorityDeque[T] extends Deque[T] with PriorityQueue[T]

trait MergeablePriorityQueue[T] extends PriorityQueue[T] {

  /** Merges two priority queues into one. */
  def merge(that: MergeablePriorityQueue[T]): MergeablePriorityQueue[T]

}
