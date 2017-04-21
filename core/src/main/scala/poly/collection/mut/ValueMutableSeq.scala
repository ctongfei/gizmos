package poly.collection.mut

import poly.collection._

/**
 * Represents a sequence whose data can be mutated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableSeq[T] extends Seq[T] { self =>

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
  def swap_!(i: Int, j: Int): Unit = {
    val t = this(i)
    this(i) = this(j)
    this(j) = t
  }

  /** Transforms each element of this sequence by the given function in place. */
  def map_!(f: T => T): Unit = {
    for (i <- Range(length))
      update(i, f(apply(i)))
  }

  /** Reverses this sequence in place. */
  def reverse_!(): Unit
}
