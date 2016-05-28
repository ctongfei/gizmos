package poly.collection.mut

import poly.collection._

/**
 * Represents a sequence whose data can be mutated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableSeq[T] extends Seq[T] with ValueMutableMap[Int, T] { self ⇒

  /**
   * Updates the element at the specific location.
   * @param i Index
   * @param x New element
   */
  def update(i: Int, x: T): Unit

  /**
   * Swaps two elements in this sequence in-place.
   * @param i Index of the first element
   * @param j Index of the second element
   */
  def swapInplace(i: Int, j: Int): Unit = {
    val t = this(i)
    this(i) = this(j)
    this(j) = t
  }

  //def inplaceReverse(): Unit
}
