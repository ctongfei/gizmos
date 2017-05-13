package poly.collection

import poly.algebra.specgroup._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IntervalSet[@sp(di) R] extends Set[Interval[R]] {

  /**
   * Returns all intervals in this set that covers the given point.
   */
  def keysCovering(x: R): Iterable[Interval[R]]

  /**
   * Returns all intervals in this set that overlaps with the given interval.
   */
  def keysOverlappingWith(r: Interval[R]): Iterable[Interval[R]]

}
