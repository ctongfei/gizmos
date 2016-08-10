package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
class ArrayTable[T] private(private val nr: Int, private val nc: Int, private val data: Array[T]) extends KeyMutableTable[T] {
  def apply(i: Int, j: Int) = data(i * nr + j)

  def numRows = nr

  def numCols = nc

  def clear() = ???
  def appendRowInplace(row: Seq[T]) = ???
  def appendColInplace(col: Seq[T]) = ???
  def removeRowAt(i: Int) = ???
  def removeColAt(j: Int) = ???
  def update(i: Int, j: Int, x: T) = ???
}
