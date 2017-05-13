package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait MutableSortedIndexedSeq[T] extends SortedIndexedSeq[T] {

  def add_!(x: T): Unit

  def remove_!(x: T): Unit

  def deleteAt_!(i: Int): Unit

  def clear_!(): Unit

}
