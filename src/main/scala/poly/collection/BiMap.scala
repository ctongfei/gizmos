package poly.collection

import poly.algebra._

/**
 * Represents a bidirectional map, which is used to model one-to-one correspondence.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiMap[K, V] extends Map[K, V] with Bijection[K, V] { self =>

  def invert(v: V): K
  
  def invertOption(v: V): Option[K]

  def containsValue(v: V): Boolean
  
  /** Returns a map that maps values to keys. */
  override def inverse: BiMap[V, K] = new BiMap[V, K] {
    def containsValue(k: K): Boolean = self.contains(k)
    def invert(k: K): V = self(k)
    def invertOption(k: K): Option[V] = self.applyOption(k)
    def applyOption(v: V): Option[K] = self.invertOption(v)
    def pairs: Enumerable[(V, K)] = self.pairs.map(_.swap)
    def apply(v: V): K = self.invert(v)
    def contains(v: V): Boolean = self.containsValue(v)
  }

}
