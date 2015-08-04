package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SortedArraySeq[T] private(private val data: SortedArray[T]) extends MutableIndexedSortedSeq[T] {

  val order = data.order

  def length = data.length

  def add(x: T) = data.add(x)

  def apply(i: Int) = data.apply(i)

  def remove(x: T) = data.remove(x)

  def deleteAt(i: Int) = data.deleteAt(i)

}

object SortedArraySeq extends CollectionFactoryWithOrder[SortedArraySeq] {

  def newBuilder[T:WeakOrder]: Builder[T, SortedArraySeq[T]] = new Builder[T, SortedArraySeq[T]] {
    val ra = new ResizableArray[T]()
    def sizeHint(n: Int) = ra.ensureCapacity(n)
    def +=(x: T) = ra.inplaceAppend(x)
    def result: SortedArraySeq[T] = {
      ra.inplaceSort()
      new SortedArraySeq[T](new SortedArray[T](ra))
    }
  }

}
