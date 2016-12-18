package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class SortedArraySeq[T] private[poly](private[poly] val data: ResizableSeq[T])(implicit val elementOrder: Order[T]) extends MutableSortedIndexedSeq[T] {

  def fastLength = data.fastLength

  def add(x: T) = data.insert_!(lowerBound(x), x)

  def fastApply(i: Int) = data.apply(i)

  def remove(x: T) = binarySearch(x) foreach data.delete_!

  def deleteAt(i: Int) = data.delete_!(i)

  def clear() = data.clear_!()

}

object SortedArraySeq extends BuilderFactory1Ev1[SortedArraySeq, Order] {

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
