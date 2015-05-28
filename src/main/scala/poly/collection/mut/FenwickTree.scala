package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._
import scala.reflect._

/**
 * A Fenwick tree.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class FenwickTree[T] private(private val data: ResizableArray[T])(implicit G: AdditiveGroup[T]) {

  val additiveGroup = G

  /**
   * Returns an element from this Fenwick tree. This operation has O(log n) complexity.
   * @param i Index
   * @return
   */
  def apply(i: Int): T = {
    var idx = i
    var sum = data(i)
    if (idx > 0) {
      val z = idx - FenwickTree.lowBit(idx)
      idx -= 1
      while (idx != z) {
        sum -= data(idx)
        idx -= FenwickTree.lowBit(idx)
      }
    }
    sum
  }

  def cumulativeSum(i: Int) = {
    var sum = additiveGroup.zero
    var idx = i
    while (idx > 0) {
      sum += data(idx)
      idx -= FenwickTree.lowBit(idx)
    }
    sum
  }

  def rangeSum(i: Int, j: Int) = cumulativeSum(j) - cumulativeSum(i - 1) //TODO: index! (inclusive or exclusive?)

  def increase(i: Int, delta: T) = {
    var idx = i
    while (idx < data.length) {
      data(idx) += delta
      idx += FenwickTree.lowBit(idx)
    }
  }
  
  def update(i: Int, x: T) = {
    val delta = x - apply(i)
    increase(i, delta)
  }

}

object FenwickTree extends AdditiveGroupCollectionFactory[FenwickTree] {

  @inline private def lowBit(x: Int) = x & -x

  implicit def newBuilder[T:ClassTag:AdditiveGroup]: CollectionBuilder[T, FenwickTree] = new CollectionBuilder[T, FenwickTree] {
    val coll = new ResizableArray[T]()
    val G = implicitly[AdditiveGroup[T]]
    def sizeHint(n: Int): Unit = coll.ensureCapacity(n)
    def +=(x: T): Unit = coll.append(x)
    def result: FenwickTree[T] = ???
  }

}
