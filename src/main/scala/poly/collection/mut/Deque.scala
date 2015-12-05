package poly.collection.mut

/**
 * Represents a double-ended queue.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Deque[T] extends Queue[T] {

  /** Returns the bottom element of the queue. */
  def bottom: T

  def popTop(): T

  def popBottom(): T

  def pop(): T = popTop()

  def dequeueFront() = popTop()
  def dequeueBack() = popBottom()
  def back = bottom

}
