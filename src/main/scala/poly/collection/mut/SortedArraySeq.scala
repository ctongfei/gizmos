package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * @author Tongfei Chen
 */
class SortedArraySeq[T] private(private val data: SortedArray[T]) extends MutableSortedIndexedSeq[T] {

  val orderOnValue = data.orderOnValue

  def fastLength = data.fastLength

  def add(x: T) = data.add(x)

  def fastApply(i: Int) = data.apply(i)

  def remove(x: T) = data.remove(x)

  def deleteAt(i: Int) = data.deleteAt(i)

}

object SortedArraySeq extends CollectionFactoryWithOrder[SortedArraySeq] {

  def newBuilder[T:WeakOrder]: Builder[T, SortedArraySeq[T]] = new Builder[T, SortedArraySeq[T]] {
    val ra = new ResizableSeq[T]()
    def sizeHint(n: Int) = ra.ensureCapacity(n)
    def add(x: T) = ra.appendInplace(x)
    def result: SortedArraySeq[T] = {
      ra.sortInplace()
      new SortedArraySeq[T](new SortedArray[T](ra))
    }
  }

}
