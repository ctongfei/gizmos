package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SortedArraySet[T] private(private val data: SortedArray[T]) extends MutableSet[T] {

  val order = data.order

  def contains(x: T) = data.tryBinarySearch(x) > 0

  def newEnumerator = data.newEnumerator

  def add(x: T) = data.add(x)

  def remove(x: T) = data.remove(x)
}
