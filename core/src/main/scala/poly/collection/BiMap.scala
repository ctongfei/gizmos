package poly.collection

import cats.kernel._

/**
 * Represents a bijective map whose key-value pairs can be iterated.
 * This can be used to model one-to-one correspondence.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiMap[K, V] extends Map[K, V] with Bijection[K, V] { self =>

  /** Returns the set of values in this map. */
  def valueSet: Set[V]

  /** Returns the equivalence relation on the value set of this bijective map. */
  implicit def valueEq: Eq[V] = valueSet.keyEq

  /** Gets the corresponding key of a given value. */
  def invert(v: V): K

  /**
   * Optionally retrieves the key associated with the specified value.
   * @param v The given value
   * @return The associated key. If the value is not found, [[None]] will be returned.
   */
  def invertOption(v: V): Option[K]

  /** Checks if the specified value is present in this map. */
  def containsValue(v: V) = valueSet.containsKey(v)

  // HELPER FUNCTIONS

  /** Returns the inverse map that maps values to keys. $LAZY */
  override def inverse: BiMap[V, K] = new BiMapT.Inverse(self)

  /**
   * Pipes two bijective maps, this coming first.
   * {{{
   *  K <=> V           V <=> W         K <=> W
   *    self   andThen   that     ==    result
   * }}}
   */
  def andThen[W](that: BiMap[V, W]): BiMap[K, W] = new BiMapT.AndThen(self, that)

  /**
   * Pipes two bijective maps, this coming last.
   * {{{
   *  K <=> V           J <=> K         J <=> W
   *    self   compose   that     ==    result
   * }}}
   */
  def compose[J](that: BiMap[J, K]): BiMap[J, V] = new BiMapT.AndThen(that, self)

  /** Alias to `andThen`. */
  def map[W](that: BiMap[V, W]): BiMap[K, W] = self andThen that

  /** Alias to `compose`. */
  def contramap[J](that: BiMap[J, K]): BiMap[J, V] = self compose that

  /**
   * Wraps the values of this bijection map with a bijection.
   * {{{
   *   K <=> V      V <=> W         K <=> W
   *    self   map   that     ==    result
   * }}}
   */
  override def map[W](f: Bijection[V, W]): BiMap[K, W] = new BiMapT.BijectivelyMapped(self, f)


  /**
   * Wraps the keys of this bijection map with a bijection.
   * {{{
   *   K <=> V            J <=> K         J <=> V
   *    self   contramap   that     ==    result
   * }}}
   */
  override def contramap[J](f: Bijection[J, K]): BiMap[J, V] = new BiMapT.BijectivelyContramapped(self, f)

  override def toString = "{" + pairs.map { case (k, v) => s"$k <-> $v" }.buildString(", ") + "}"

}

abstract class AbstractBiMap[K, V] extends AbstractMap[K, V] with BiMap[K, V]

private[poly] object BiMapT {

  class Inverse[K, V](self: BiMap[K, V]) extends AbstractBiMap[V, K] {
    def keySet = self.valueSet
    def valueSet = self.keySet
    def invert(k: K) = self(k)
    def invertOption(k: K) = self ? k
    def apply(v: V) = self.invert(v)
    def ?(v: V) = self.invertOption(v)
    override def pairs = self.pairs.map(_.swap)
    override def size = self.size
    override def inverse = self
  }

  class AndThen[K, V, W](self: BiMap[K, V], that: BiMap[V, W]) extends AbstractBiMap[K, W] {
    def keySet = self.keySet filter { k => that containsKey self(k) }
    def valueSet = that.valueSet filter { w => self containsValue that.invert(w) }
    def invert(w: W) = self.invert(that.invert(w))
    def invertOption(w: W) = that.invertOption(w) map self.invert
    def apply(x: K) = that(self(x))
    def ?(k: K) = (self ? k) map that
  }

  class BijectivelyMapped[K, V, W](self: BiMap[K, V], f: Bijection[V, W]) extends AbstractBiMap[K, W] {
    def keySet = self.keySet
    def valueSet = self.valueSet map f
    def invert(w: W) = self.invert(f.invert(w))
    def invertOption(w: W) = self.invertOption(f.invert(w))
    def apply(x: K) = f(self(x))
    def ?(k: K) = (self ? k) map f
  }

  class BijectivelyContramapped[J, K, V](self: BiMap[K, V], f: Bijection[J, K]) extends AbstractBiMap[J, V] {
    def valueSet = self.valueSet
    def invert(v: V) = f.invert(self.invert(v))
    def invertOption(v: V) = self invertOption v map f.invert
    def apply(x: J) = self(f(x))
    def keySet = self.keySet contramap f
    def ?(k: J) = self ? f(k)
  }

}