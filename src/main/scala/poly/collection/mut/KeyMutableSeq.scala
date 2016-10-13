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
  def prepend_!(x: T): Unit

  /**
   * Appends the specific element to the end of the sequence.
   * @param x The element to be appended
   */
  def append_!(x: T): Unit

  /**
   * Clears this sequence.
   */
  def clear_!(): Unit

  /**
   * Inserts an element at the specific position.
   * @param i Position for insertion
   * @param x The element to be inserted
   */
  def insert_!(i: Int, x: T): Unit

  /**
   * Deletes the element at the specific position.
   * @param i Position for deletion
   */
  def delete_!(i: Int): Unit

  final def :+=(x: T) = append_!(x)

  final def =+:(x: T) = prepend_!(x)

}
