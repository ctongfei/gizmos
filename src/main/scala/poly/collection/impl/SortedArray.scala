package poly.collection.impl

import poly.algebra._
import poly.algebra.ops._
import poly.collection._

/**
 * A resizable sorted array. This serves as the implementation container of `SortedArrayX` classes.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class SortedArray[T] private(val data: ResizableArray[T])(implicit O: WeakOrder[T]) extends SortedIndexedSeq[T] {

  val order = O

  def length = data.length

  def add(x: T) = data.insertAt(lowerBound(x), x)

  def remove(x: T) = data.deleteAt(lowerBound(x))


}
