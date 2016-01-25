package poly.collection.mut

import poly.algebra._
import poly.collection.builder._

import scala.language.higherKinds

/**
  * @author Tongfei Chen
  */
class DistinctPriorityQueue[Q[α] <: PriorityQueue[α], T] private(stateEquiv: Equiv[T], private val inner: Q[T]) extends PriorityQueue[T] {

  private[this] val seen = HashMap[T, T]() // !!(stateEquiv)

  def order = inner.order

  def push(x: T) = if (seen notContainsKey x) {
    inner push x
    seen add (x, x)
  } else if (order.lt(x, seen(x))) {
    inner push x
    seen(x) = x
  }

  def top = inner.top

  def pop() = inner.pop()

  def elements = inner.elements
}

object DistinctPriorityQueue {
  def apply[Q[α] <: PriorityQueue[α], T](xs: T*)(stateEquiv: Equiv[T])(implicit b: Builder[T, Q[T]]): DistinctPriorityQueue[Q, T] = {
    xs foreach b.add
    new DistinctPriorityQueue[Q, T](stateEquiv, b.result)
  }
}
