package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * A Fenwick tree.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class FenwickTree[T] private(private val data: ResizableSeq[T])
  (implicit val additiveGroup: AdditiveGroup[T]) extends IndexedSeq[T]
{

  import FenwickTree._

  def fastLength = data.fastLength

  /**
   * Returns an element from this Fenwick tree. This operation has O(log n) complexity.
 *
   * @param i Index
   * @return
   */
  def fastApply(i: Int): T = {
    var idx = i
    var sum = data(i)
    if (idx > 0) {
      val z = idx - lowBit(idx)
      idx -= 1
      while (idx != z) {
        sum -= data(idx)
        idx -= lowBit(idx)
      }
    }
    sum
  }

  /** Returns the sum of the elements in the range [0, i). */
  def cumulativeSum(i: Int) = {
    var sum = additiveGroup.zero
    var idx = i - 1
    while (idx > 0) {
      sum += data(idx)
      idx -= lowBit(idx)
    }
    sum
  }

  /** Returns the sum of the elements in the range [i, j). */
  def rangeSum(i: Int, j: Int) = cumulativeSum(j) - cumulativeSum(i) //TODO: index! (inclusive or exclusive?)

  def increment(i: Int, delta: T) = {
    var idx = i
    while (idx < data.fastLength) {
      data(idx) += delta
      idx += lowBit(idx)
    }
  }
  
  def update(i: Int, x: T) = {
    val delta = x - apply(i)
    increment(i, delta)
  }

}

object FenwickTree extends CollectionFactoryWithAdditiveGroup[FenwickTree] {

  @inline private def lowBit(x: Int) = x & -x

  implicit def newBuilder[T:AdditiveGroup]: Builder[T, FenwickTree[T]] = new Builder[T, FenwickTree[T]] {
    val coll = new ResizableSeq[T]()
    val G = implicitly[AdditiveGroup[T]]
    def sizeHint(n: Int): Unit = coll.ensureCapacity(n)
    def add(x: T): Unit = coll.appendInplace(x)
    def result: FenwickTree[T] = ??? //TODO:!!! not implemented
  }

}
