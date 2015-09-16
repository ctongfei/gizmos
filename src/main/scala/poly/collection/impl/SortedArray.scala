package poly.collection.impl

import poly.algebra._
import poly.collection._
import poly.collection.exception._

/**
 * A resizable sorted array. This serves as the implementation container of `SortedArrayX` classes.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SortedArray[T] private[poly](val data: ResizableSeq[T])(implicit val order: WeakOrder[T])
  extends IndexedSortedSeq[T]
{

  def fastLength = data.fastLength

  def fastApply(i: Int) = data(i)

  def add(x: T) = data.insertAt(lowerBound(x), x)

  def deleteAt(i: Int) = data.deleteAt(i)

  def remove(x: T) = binarySearch(x) match {
    case Some(i) => data.deleteAt(i)
    case None =>
  }

}
