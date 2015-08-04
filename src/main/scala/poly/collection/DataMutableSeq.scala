package poly.collection

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
  def inplaceSwap(i: Int, j: Int): Unit = {
    val t = this(i)
    this(i) = this(j)
    this(j) = t
  }

  //def inplaceReverse(): Unit
  def inplaceMap(f: T => T): Unit
}
