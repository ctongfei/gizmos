package poly.collection

import poly.algebra._
import poly.algebra.hkt._

/**
 * Represents a bijective map, which is used to model one-to-one correspondence.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait BijectiveMap[K, V] extends Map[K, V] with Bijection[K, V] { self =>

  def invert(v: V): K
  
  def invertOption(v: V): Option[K]

  def containsValue(v: V): Boolean
  
  /** Returns a map that maps values to keys. */
  override def inverse: BijectiveMap[V, K] = new BijectiveMap[V, K] {
    def invert(k: K) = self(k)
    def invertOption(k: K) = self ? k
    def ?(v: V) = self.invertOption(v)
    def pairs: Iterable[(V, K)] = self.pairs.map(_.swap)
    def size = self.size
    def apply(v: V) = self.invert(v)
    def containsKey(v: V) = self.containsValue(v)
    def containsValue(k: K) = self.containsKey(k)
    override def inverse = self
  }

  def map[W](that: BijectiveMap[V, W]): BijectiveMap[K, W] = new BijectiveMap[K, W] {
    def apply(k: K) = that(self(k))
    def ?(k: K) = for (v ← self ? k; w ← that ? v) yield w
    def invert(w: W) = self.invert(that.invert(w))
    def invertOption(w: W) = for (v ← that.invertOption(w); k ← self.invertOption(v)) yield k
    def pairs: Iterable[(K, W)] = for (k ← keys; v ← self ? k; w ← that ? v) yield (k, w)
    def size = pairs.size
    def containsKey(k: K) = (this ? k).isDefined
    def containsValue(w: W) = this.invertOption(w).isDefined
  }

  def andThen[W](that: BijectiveMap[V, W]) = this map that
  def compose[J](that: BijectiveMap[J, K]) = that map this


}
