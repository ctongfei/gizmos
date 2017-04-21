package poly.collection.mut

import poly.collection._

/**
 * Represents a priority queue in which elements can be accessed by keys.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyedPriorityQueue[K, T] extends PriorityQueue[T] {

  def keyElementMap: Map[K, T]

  def keySelector: T => K

  def pairs = elements map { e => keySelector(e) -> e }

}
