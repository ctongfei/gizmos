package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Multimap[K, V] extends Enumerable[(K, V)] {

  def get(x: K): Set[V]

  def apply(x: K): Set[V]



}
