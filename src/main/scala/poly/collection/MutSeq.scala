package poly.collection

import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait ImmutableSeq[T] extends Seq[T]

/**
 * Represents a sequence in which data can be mutated.
 * @tparam T
 */
trait DataMutableSeq[T] extends Seq[T] {

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
  def swap(i: Int, j: Int): Unit = {
    val t = this(i)
    this(i) = this(j)
    this(j) = t
  }

  def inplaceReverse(): Unit
  def inplaceMap(f: T => T): Unit
}

/**
 * Represents a structurally-mutable sequence.
 * @tparam T
 */
trait StructureMutableSeq[T] extends DataMutableSeq[T] {
  /**
   * Prepends the specific element to the start of the sequence.
   * @param x The element to be prepended
   */
  def prepend(x: T): Unit

  /**
   * Appends the specific element to the end of the sequence.
   * @param x The element to be appended
   */
  def append(x: T): Unit

  /**
   * Clears this sequence.
   */
  def clear(): Unit

  /**
   * Inserts an element at the specific position.
   * @param i Position for insertion
   * @param x The element to be inserted
   */
  def insertAt(i: Int, x: T): Unit

  /**
   * Deletes the element at the specific position.
   * @param i Position for deletion
   */
  def deleteAt(i: Int): Unit

}
