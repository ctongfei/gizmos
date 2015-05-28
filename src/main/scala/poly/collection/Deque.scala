package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Deque[T] extends Queue[T] {

  /**
   * Returns the bottom element of the queue.
   * @return
   */
  def bottom: T

  def popTop(): T

  def popBottom(): T

  def pop(): T = popTop()

  def dequeueFront() = popTop()
  def dequeueBack() = popBottom()
  def back = bottom

}
