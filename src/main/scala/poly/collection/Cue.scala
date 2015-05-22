package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
//TODO: conform to Enumerable
trait Cue[T] {

  /** Checks if this queue is empty. */
  def isEmpty: Boolean = size == 0

  /**
   * Pushes the specific element into this queue.
   * @param x The element to be pushed
   */
  def enqueue(x: T): Unit

  /**
   * Returns the top element of the queue.
   * @return The top element
   */
  def front: T

  /**
   * Removes the top element from the queue and returns it.
   * @return The removed element
   */
  def dequeue(): T

  /**
   * Returns the number of elements in this queue.
   * @return The number of elements
   */
  def size: Int

  def +=(x: T): Unit = enqueue(x)
  def ++=(xs: Traversable[T]) = xs foreach +=

}

trait Stack[T] extends Cue[T] {
  def push(x: T): Unit
  def top: T
  def pop(): T

  def enqueue(x: T) = push(x)
  def front = top
  def dequeue() = pop()
}
