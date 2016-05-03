package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.builder._
import scala.language.higherKinds

/**
  * @author Tongfei Chen
  */
class DistinctPriorityQueue[Q[α] <: PriorityQueue[α], T: Eq] private(private val inner: Q[T]) extends PriorityQueue[T] {

  private[this] val seen = AutoMap[T, T]()

  implicit def orderOnElements = inner.orderOnElements

  def push(x: T) = if (seen notContainsKey x) {
    inner push x
    seen addInplace (x, x)
  } else if (x < seen(x)) {
    inner push x
    seen(x) = x
  }

  def top = inner.top

  def pop() = inner.pop()

  def elements = inner.elements

}

object DistinctPriorityQueue {
  def apply[Q[α] <: PriorityQueue[α], T: Eq](xs: T*)(implicit b: Builder[T, Q[T]]): DistinctPriorityQueue[Q, T] = {
    xs foreach b.addInplace
    new DistinctPriorityQueue[Q, T](b.result)
  }
}
