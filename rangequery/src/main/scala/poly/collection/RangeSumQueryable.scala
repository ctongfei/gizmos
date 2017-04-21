package poly.collection

import poly.algebra._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RangeSumQueryable[T] extends IndexedSeq[T] {

  def additiveGroup: AdditiveGroup[T]

  def prefixSum(i: Int): T

  def rangeSum(i: Int, j: Int): T

}
