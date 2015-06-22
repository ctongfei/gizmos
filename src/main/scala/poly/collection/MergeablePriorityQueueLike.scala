package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MergeablePriorityQueueLike[T, Repr] extends PriorityQueue[T] {

  /** Merges two priority queues into one. */
  def merge(that: Repr): Repr

}
