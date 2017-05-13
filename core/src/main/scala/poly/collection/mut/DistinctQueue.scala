package poly.collection.mut

import poly.collection._
import scala.language.higherKinds

/**
 * Represents a queue whose elements are kept distinct.
 * @author Tongfei Chen
 */
class DistinctQueue[T: Eq] private(private val inner: Queue[T]) extends Queue[T] {

  private[this] val seen = AutoSet[T]()

  def enqueue(x: T) = if (!seen(x)) {
    inner += x
    seen += x
  }

  override def enqueueAll(xs: Traversable[T]) = {
    val buf = ArraySeq[T]()
    for (x <- xs)
      if (!seen(x)) {
        seen += x
        buf :+= x
      } // buffers succeeding elements and push in batch
    inner ++= buf
  }

  def front = inner.front

  def dequeue() = inner.dequeue()

  def elements = inner.elements

}

object DistinctQueue {
  def apply[T: Eq](inner: Queue[T]): DistinctQueue[T] = {
    new DistinctQueue(inner)
  }
}
