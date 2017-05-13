package poly.collection.mut

import poly.collection._
import poly.collection.factory._

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

object SortedArraySet extends SetFactory[SortedArraySet, Order] {
  def newSetBuilder[T: Order] = SortedArraySeq.newBuilder.map(r => new SortedArraySet(r))
}
