package poly.collection.mut

/**
 * An array backed by a contiguous array.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ArrayTable[T] private(private val nr: Int, private val nc: Int, private val data: Array[T]) extends ValueMutableTable[T] {
  def apply(i: Int, j: Int) = data(i * nc + j)

  def numRows = nr

  def numCols = nc

  def update(i: Int, j: Int, x: T) = data(i * nc + j) = x
}
