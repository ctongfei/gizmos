package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableTable[T] extends ValueMutableTable[T] {
  def clear_!(): Unit
  def appendRow_!(row: Seq[T]): Unit
  def appendCol_!(col: Seq[T]): Unit
  def removeRow_!(i: Int): Unit
  def removeCol_!(j: Int): Unit
}
