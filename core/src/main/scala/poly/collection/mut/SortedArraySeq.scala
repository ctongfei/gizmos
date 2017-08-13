package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class SortedArraySeq[T] private[poly](private[poly] val data: ResizableSeq[T])(implicit val elementOrder: Order[T]) extends MutableSortedIndexedSeq[T] {

  def fastLength = data.fastLength

  def add_!(x: T) = data.insert_!(indexOfLowerBound(x), x)

  def fastApply(i: Int) = data.apply(i)

  def remove_!(x: T) = binarySearch(x) match {
    case Right(i) => data.delete_!(i)
    case _ => /* do nothing */
  }

  def deleteAt_!(i: Int) = data.delete_!(i)

  def clear_!() = data.clear_!()

}

object SortedArraySeq extends Factory1[Id, SortedArraySeq, Order] {

  def newBuilder[T: Order]: Builder[T, SortedArraySeq[T]] = new Builder[T, SortedArraySeq[T]] {
    val ra = new ResizableSeq[T]()
    override def sizeHint(n: Int) = ra.ensureCapacity(n)
    def add(x: T) = ra.append_!(x)
    def result: SortedArraySeq[T] = {
      ra.sort_!()
      new SortedArraySeq[T](ra)
    }
  }

}
