package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutSeq[T] extends Seq[T] {

  /**
   * Prepends the specific element to the start of the sequence.
   * @param x The element
   */
  def prepend(x: T): Unit

  def append(x: T): Unit

  def update(i: Int, x: T): Unit

  def clear(): Unit

  def insertAt(i: Int, x: T): Unit

  def deleteAt(i: Int): Unit

  def swap(i: Int, j: Int): Unit

  def inplaceReverse(): Unit

  def inplaceMap(f: T => T): Unit

}
