package poly.collection.impl.specialized

import poly.collection.specgroup._
import poly.macroutil._
import scala.reflect._

/**
 * A specialized version for array tables.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class SpArrayTable[@sp(Int, Boolean) T: ClassTag]
(val numRows: Int, val numCols: Int) {

  private[this] val data: Array[T] = Array.ofDim[T](numRows * numCols)

  def apply(i: Int, j: Int) = data(i * numCols + j)

  def update(i: Int, j: Int, x: T) = data(i * numCols + j) = x

  def fillInplace(x: T) = FastLoop.ascending(0, numRows * numCols, 1) { i =>
    data(i) = x
  }

}
