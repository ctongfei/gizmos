package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait KeyMutableTable[T] extends DataMutableTable[T] {
  def clear(): Unit
  def appendRowInplace(row: Seq[T]): Unit
  def appendColInplace(col: Seq[T]): Unit
  def removeRowAt(i: Int): Unit
  def removeColAt(j: Int): Unit
}
