package poly.collection.mut

/**
 * Represents a structurally-mutable sequence.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait KeyMutableSeq[T] extends ValueMutableSeq[T] {
  /**
   * Prepends the specific element to the start of the sequence.
   * @param x The element to be prepended
   */
  def prependInplace(x: T): Unit

  /**
   * Appends the specific element to the end of the sequence.
   * @param x The element to be appended
   */
  def appendInplace(x: T): Unit

  /**
   * Clears this sequence.
   */
  def clear(): Unit

  /**
   * Inserts an element at the specific position.
   * @param i Position for insertion
   * @param x The element to be inserted
   */
  def insertInplace(i: Int, x: T): Unit

  /**
   * Deletes the element at the specific position.
   * @param i Position for deletion
   */
  def deleteInplace(i: Int): Unit

  final def :+=(x: T) = appendInplace(x)

  final def =+:(x: T) = prependInplace(x)

}
