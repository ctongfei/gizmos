package poly.collection

import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait ImmutableSeq[T] extends Seq[T]

trait DMutSeq[@sp(fdi) T] extends Seq[T] {
  def update(i: Int, x: T): Unit
  def swap(i: Int, j: Int): Unit
  def inplaceReverse(): Unit
  def inplaceMap(f: T => T): Unit
}

trait SMutSeq[@sp(fdi) T] extends DMutSeq[T] {
  /**
   * Prepends the specific element to the start of the sequence.
   * @param x The element
   */
  def prepend(x: T): Unit

  def append(x: T): Unit

  def clear(): Unit

  def insertAt(i: Int, x: T): Unit

  def deleteAt(i: Int): Unit

}
