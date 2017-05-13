package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RangeMonoidQueryable[T] extends IndexedSeq[T] {

  def monoid: Monoid[T]

  /**
   * $Ologn Computes the aggregate value of the prefix [0, ''r'') using the monoid designated in this structure.
   * Common use cases include:
   * <ul>
   *   <li> Range sum query (RSQ): Computes the sum of the prefix given an additive monoid. </li>
   *   <li> Range minimum query (RMQ): Computes the minimum of value in the prefix given a lower semilattice. </li>
   * </ul>
   * @note This is a faster way of computing `this.take(r).foldByMonoid`.
   */
  def prefixAggregate(r: Int): T

  /**
   * $Ologn Computes the aggregate value of the slice [''l'', ''r'') using the monoid designated in this structure.
   * Common use cases include:
   * <ul>
   *   <li> Range sum query (RSQ): Computes the sum of the slice given an additive monoid. </li>
   *   <li> Range minimum query (RMQ): Computes the minimum of value in the slice given a lower semilattice. </li>
   * </ul>
   * @note This is a faster way of computing `this.slice(l, r).foldByMonoid`.
   */
  def rangeAggregate(l: Int, r: Int): T

}
