package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DoubleEndedQueue[T] extends Queue[T] {

  /**
   * Returns the bottom element of the queue.
   * @return
   */
  def back: T

  def popFront(): T

  def popBack(): T

  def pop(): T = popFront()

}
