package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class SortedArraySeq[T] private(private[poly] val data: ResizableSeq[T])(implicit val orderOnElements: Order[T]) extends MutableSortedIndexedSeq[T] {

  def fastLength = data.fastLength

  def add(x: T) = data.insertInplace(lowerBound(x), x)

  def fastApply(i: Int) = data.apply(i)

  def remove(x: T) = binarySearch(x) foreach data.deleteInplace

  def deleteAt(i: Int) = data.deleteInplace(i)

}

object SortedArraySeq extends BuilderFactoryEv[SortedArraySeq, Order] {

  def newBuilder[T: Order]: Builder[T, SortedArraySeq[T]] = new Builder[T, SortedArraySeq[T]] {
    val ra = new ResizableSeq[T]()
    override def sizeHint(n: Int) = ra.ensureCapacity(n)
    def addInplace(x: T) = ra.appendInplace(x)
    def result: SortedArraySeq[T] = {
      ra.sortInplace()
      new SortedArraySeq[T](ra)
    }
  }

}
