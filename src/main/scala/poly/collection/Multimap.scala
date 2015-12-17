package poly.collection

/**
 * @author Tongfei Chen
 */
trait Multimap[K, V] extends Iterable[(K, V)] {

  def get(x: K): Set[V]

  def apply(x: K): Set[V]


}
