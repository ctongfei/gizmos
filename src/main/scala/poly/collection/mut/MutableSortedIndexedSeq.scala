package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait MutableSortedIndexedSeq[T] extends SortedIndexedSeq[T] {

  def add(x: T): Unit

  def remove(x: T): Unit

  def deleteAt(i: Int): Unit

}
