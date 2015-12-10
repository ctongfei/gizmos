package poly.collection.mut

import poly.collection.builder._

/**
  * @author Tongfei Chen
  */
class DistinctQueue[Q[A] <: Queue[A], T] private(private val inner: Q[T]) extends Queue[T] {

  private[this] val seen = HashSet[T]()

  def push(x: T) = if (!seen(x)) {
    inner push x
    seen add x
  }

  def top = inner.top

  def pop() = inner.pop()

  def newIterator = inner.newIterator

}

object DistinctQueue {
  def apply[Q[A] <: Queue[A], T](xs: T*)(implicit b: Builder[T, Q[T]]): DistinctQueue[Q, T] = {
    new DistinctQueue[Q, T](b.result)
  }
}