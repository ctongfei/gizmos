package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait DataMutableTable[T] extends Table[T] {
  def update(i: Int, j: Int, x: T): Unit
}
