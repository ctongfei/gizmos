package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ContiguousArrayTable[T](val numRows: Int, val numCols: Int, val data: Array[AnyRef]) extends Table[T] {

  def apply(i: Int, j: Int) = data(i * numCols + j).asInstanceOf[T]

}
