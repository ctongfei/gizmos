package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class SortedArraySet[T] private(private val data: SortedArraySeq[T]) extends SortedSet[T] with KeyMutableSet[T] {

  override def keyOrder = data.elementOrder

  def contains(x: T) = data.tryBinarySearch(x) >= 0

  def keys = data

  def add_!(x: T) = data.add(x)

  def remove_!(x: T) = data.remove(x)

  override def size: Int = data.size

  def clear_!() = data.data.clear_!()
}

object SortedArraySet extends BuilderFactoryA_EvA[SortedArraySet, Order] {
  implicit def newBuilder[T: Order]: Builder[T, SortedArraySet[T]] = new Builder[T, SortedArraySet[T]] {
    private[this] val b = SortedArraySeq.newBuilder
    def add(x: T) = b += x
    def result = new SortedArraySet(b.result)
    override def sizeHint(n: Int) = b.sizeHint(n)
  }
}
