package poly.collection.mut

import poly.collection._

/**
 * Represents a mutable structure where elements can be inserted, but at every time only one element can be removed.
 * This encapsulates the following data structures:
 * <ul>
 *   <li> Stacks ([[ArrayStack]], [[ListStack]]): LIFO (last-in first-out) queues; </li>
 *   <li> Queues ([[ArrayQueue]]): FIFO (first-in first-out) queues; </li>
 *   <li> Priority queues ([[BinaryHeap]], etc.): at every time the removed element is the one associated with the least priority values; </li>
 *   <li> Random queues ([[RandomQueue]]): at every tiem a random element is to be removed.
 * </ul>
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Queue[T] {

  /** Returns an iterable list of all the elements in this queue. */
  def elements: Iterable[T]

  /** Checks if this queue is empty. */
  def isEmpty: Boolean = size == 0

  /** Checks if this queue is not empty. */
  def notEmpty: Boolean = size != 0

  /** Pushes the specified element into this queue. */
  def enqueue(x: T): Unit

  /** Returns the top element of the queue. */
  def front: T

  /** Removes the top element from the queue and returns it. */
  def dequeue(): T

  /** Returns the number of elements in this queue. */
  def size: Int = elements.size

  /** Pushes the specified element into this queue. */
  final def +=(x: T): Unit = enqueue(x)

  /** Pushes all the specified elements into this queue. */
  def enqueueAll(xs: Traversable[T]) = xs foreach +=

  /** Pushes all the specified elements into this queue. */
  final def ++=(xs: Traversable[T]) = enqueueAll(xs)

  final def push(x: T) = enqueue(x)
  final def pop() = dequeue()
  final def pushAll(xs: Traversable[T]) = enqueueAll(xs)

  override def toString = "[" + elements.toString0 + "]"

}
