package poly.collection

import poly.algebra._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SortedMap[K, V] extends Map[K, V] { self =>

  def order: WeakOrder[K]

  def pairs: SortedIterable[(K, V)]

}
