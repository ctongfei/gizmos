package poly.collection

import poly.algebra._

/**
 * Represents a bijective map whose key-value pairs can be iterated.
 * This is used to model one-to-one correspondence.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BijectiveMap[K, V] extends Map[K, V] with Bijection[K, V] { self =>

  /** Returns the equivalence relation on the value set of this bijective map. */
  def equivOnValue: Equiv[V]

  /** Gets the corresponding key of a given value. */
  def invert(v: V): K

  def invertOption(v: V): Option[K]

  def containsValue(v: V): Boolean

  // HELPER FUNCTIONS

  /** Returns the inverse map that maps values to keys. $LAZY */
  override def inverse: BijectiveMap[V, K] = new AbstractBijectiveMap[V, K] {
    def equivOnKey = self.equivOnValue
    def equivOnValue = self.equivOnKey
    def invert(k: K) = self(k)
    def invertOption(k: K) = self ? k
    def ?(v: V) = self.invertOption(v)
    def pairs: Iterable[(V, K)] = self.pairs.map(_.swap)
    override def size = self.size
    def apply(v: V) = self.invert(v)
    def containsKey(v: V) = self.containsValue(v)
    def containsValue(k: K) = self.containsKey(k)
    override def inverse = self
  }

  def map[W](that: BijectiveMap[V, W]): BijectiveMap[K, W] = new AbstractBijectiveMap[K, W] {
    def equivOnKey = self.equivOnKey
    def equivOnValue = that.equivOnValue
    def apply(k: K) = that(self(k))
    def ?(k: K) = for (v ← self ? k; w ← that ? v) yield w
    def invert(w: W) = self.invert(that.invert(w))
    def invertOption(w: W) = for (v ← that.invertOption(w); k ← self.invertOption(v)) yield k
    def pairs: Iterable[(K, W)] = for (k ← keys; v ← self ? k; w ← that ? v) yield (k, w)
    def containsKey(k: K) = (this ? k).isDefined
    def containsValue(w: W) = this.invertOption(w).isDefined
  }

  def contramap[J](that: BijectiveMap[J, K]) = that map this

  def andThen[W](that: BijectiveMap[V, W]) = this map that
  def compose[J](that: BijectiveMap[J, K]) = that map this

  def |>[W](that: BijectiveMap[V, W]) = this andThen that
  def |<[J](that: BijectiveMap[J, K]) = this compose that
}

abstract class AbstractBijectiveMap[K, V] extends AbstractMap[K, V] with BijectiveMap[K, V]

