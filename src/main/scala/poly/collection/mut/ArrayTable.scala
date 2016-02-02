package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
class ArrayTable[T] private(private val nr: Int, private val nc: Int, private val data: Array[T]) extends Table[T] {
  def apply(i: Int, j: Int) = data(i * nr + j)

  def numRows = nr

  def numCols = nc
}
