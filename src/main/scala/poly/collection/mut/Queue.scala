package poly.collection.mut

import poly.collection._

/**
 * Represents a queue.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Queue[T] {

  /** Returns an iterable list of all the elements in this queue. */
  def elements: Iterable[T]

  /** Checks if this queue is empty. $O1 */
  def isEmpty: Boolean = size == 0

  /** Checks if this queue is not empty. $O1 */
  def notEmpty: Boolean = size != 0

  /** Pushes the specified element into this queue. $O1 */
  def push(x: T): Unit

  /** Returns the top element of the queue. $O1 */
  def top: T

  /** Removes the top element from the queue and returns it. $O1 */
  def pop(): T

  /** Returns the number of elements in this queue. */
  def size: Int = elements.size

  /** Pushes the specified element into this queue. $O1 */
  def +=(x: T): Unit = push(x)

  /** Pushes all the specified elements into this queue. */
  def pushAll(xs: Traversable[T]) = xs foreach +=

  /** Pushes all the specified elements into this queue. */
  def ++=(xs: Traversable[T]) = pushAll(xs)

  def enqueue(x: T) = push(x)
  def dequeue() = pop()
  def front = top

}
