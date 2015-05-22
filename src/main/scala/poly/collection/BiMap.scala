package poly.collection

/**
 * Represents a bidirectional map, which is used to model one-to-one correspondence.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiMap[K, V] extends Map[K, V] {

  /** Returns a map that maps values to keys. */
  def inverse: Map[V, K]

}
