package poly.collection.mut

import poly.collection._
import poly.collection.builder._
import scala.language.higherKinds

/**
  * @author Tongfei Chen
  */
class DistinctQueue[Q[A] <: Queue[A], T] private(private val inner: Q[T]) extends Queue[T] {

  private[this] val seen = HashSet[T]()

  def push(x: T) = if (!seen(x)) {
    inner push x
    seen add x
  }

  override def pushAll(xs: Traversable[T]) = {
    val buf = ArraySeq[T]()
    for (x ← xs)
      if (!seen(x)) {
        seen add x
        buf appendInplace x
      }
    inner pushAll buf
  }

  def top = inner.top

  def pop() = inner.pop()

  def newIterator = inner.newIterator

}

object DistinctQueue {
  def apply[Q[A] <: Queue[A], T](xs: T*)(implicit b: Builder[T, Q[T]]): DistinctQueue[Q, T] = {
    xs foreach b.add
    new DistinctQueue[Q, T](b.result)
  }
}
