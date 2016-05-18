package poly.collection.mut

/**
 * Represents a mutable double-ended queue.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Deque[T] extends Queue[T] {

  /** Returns the bottom element of the queue. */
  def bottom: T

  def popTop(): T

  def popBottom(): T

  def pop(): T = popTop()

  final def dequeueFront() = popTop()
  final def dequeueBack() = popBottom()
  final def back = bottom

}
