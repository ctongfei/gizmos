package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class SortedArraySet[T] private(private val data: SortedArray[T]) extends SortedSet[T] with MutableSet[T] {

  override def orderOnKey = data.orderOnValue

  def contains(x: T) = data.tryBinarySearch(x) >= 0

  def elements = data

  def add(x: T) = data.add(x)

  def remove(x: T) = data.remove(x)

  override def size: Int = data.size

}
