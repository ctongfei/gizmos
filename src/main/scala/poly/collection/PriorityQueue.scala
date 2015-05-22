package poly.collection

import poly.algebra._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait PriorityQueue[T] extends Cue[T] {

  /** Retrieves the order under which the elements are sorted. */
  def order: WeakOrder[T]

}


trait DoubleEndedPriorityQueue[T] {

  def order: WeakOrder[T]

  /** Checks if this queue is empty. */
  def isEmpty: Boolean = size == 0

  /**
   * Pushes the specific element into this queue.
   * @param x The element to be pushed
   */
  def enqueue(x: T): Unit


  def min: T

  def max: T

  def dequeueMin(): T

  def dequeueMax(): T

  def size: Int

  def +=(x: T): Unit = enqueue(x)
  def ++=(xs: Traversable[T]) = xs foreach +=


}