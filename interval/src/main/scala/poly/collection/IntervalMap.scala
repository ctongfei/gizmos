package poly.collection

import poly.collection.specgroup._

/**
 * @author Tongfei Chen
 */
trait IntervalMap[@sp(di) R, +V] extends Map[Interval[R], V] {

  def keySet: IntervalSet[R]

  def pairsCovering(x: R): Iterable[(Interval[R], V)]

  def pairsOverlappingWith(r: Interval[R]): Iterable[(Interval[R], V)]

  def valuesCovering(x: R): Iterable[V]

  def valuesOverlappingWith(r: Interval[R]): Iterable[V]

}
