package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import scala.language.higherKinds

/**
  * @author Tongfei Chen
  */
class DistinctQueue[Q[α] <: Queue[α], T: Eq] private(private val inner: Q[T]) extends Queue[T] {

  private[this] val seen = AutoSet[T]()

  def push(x: T) = if (!seen(x)) {
    inner += x
    seen += x
  }

  override def pushAll(xs: Traversable[T]) = {
    val buf = ArraySeq[T]()
    for (x ← xs)
      if (!seen(x)) {
        seen += x
        buf :+= x
      } // buffers succeeding elements and push in batch
    inner ++= buf
  }

  def top = inner.top

  def pop() = inner.pop()

  def elements = inner.elements

}

object DistinctQueue {
  def apply[Q[α] <: Queue[α], T: Eq](xs: T*)(implicit b: Builder[T, Q[T]]): DistinctQueue[Q, T] = {
    xs foreach b.addInplace
    new DistinctQueue[Q, T](b.result)
  }
}
