package poly.collection

import poly.algebra._

/**
 * Represents a bijective map whose key-value pairs can be iterated.
 * This is used to model one-to-one correspondence.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiMap[K, V] extends Map[K, V] with Bijection[K, V] with Relation[K, V] { self =>

  /** Returns the equivalence relation on the value set of this bijective map. */
  def eqOnValues: Eq[V]

  /** Gets the corresponding key of a given value. */
  def invert(v: V): K

  /**
   * Optionally retrieves the key associated with the specified value.
   *
   * @param v The given value
   * @return The associated key. If the value is not found, [[None]] will be returned.
   */
  def invertOption(v: V): Option[K]

  /** Checks if the specified value is present in this map. */
  def containsValue(v: V): Boolean

  def values: Iterable[V]

  /** Returns the set of values in this map. $LAZY */
  def valueSet: Set[V] = new AbstractSet[V] {
    def contains(x: V) = self containsValue x
    def keys = self.values
    def eqOnKeys = self.eqOnValues
  }

  def related(k: K, v: V) = eqOnValues.eq(self(k), v)

  // HELPER FUNCTIONS

  /** Returns the inverse map that maps values to keys. $LAZY */
  override def inverse: BiMap[V, K] = new AbstractBiMap[V, K] {
    def eqOnKeys = self.eqOnValues
    def eqOnValues = self.eqOnKeys
    def invert(k: K) = self(k)
    def invertOption(k: K) = self ? k
    def apply(v: V) = self.invert(v)
    def ?(v: V) = self.invertOption(v)
    def keys = self.values
    def values = self.keys
    override def pairs = self.pairs.map(_.swap)
    override def size = self.size
    def containsKey(v: V) = self.containsValue(v)
    def containsValue(k: K) = self.containsKey(k)
    override def inverse = self
  }

  def map[W](that: BiMap[V, W]): BiMap[K, W] = new AbstractBiMap[K, W] {
    def eqOnKeys = self.eqOnKeys
    def eqOnValues = that.eqOnValues
    def apply(k: K) = that(self(k))
    def ?(k: K) = for (v <- self ? k; w <- that ? v) yield w
    def invert(w: W) = self.invert(that.invert(w))
    def invertOption(w: W) = for (v <- that.invertOption(w); k <- self.invertOption(v)) yield k
    def keys = self.keys //TODO: wrong!
    def values = ???
    override def pairs = for (k <- keys; v <- self ? k; w <- that ? v) yield (k, w)
    def containsKey(k: K) = (this ? k).isDefined
    def containsValue(w: W) = this.invertOption(w).isDefined
  }

  override def map[W](that: Bijection[V, W]): BiMap[K, W] = ???
  override def contramap[J](that: Bijection[J, K]): BiMap[J, V] = ???

  def contramap[J](that: BiMap[J, K]) = that map this

  def andThen[W](that: BiMap[V, W]) = this map that
  def compose[J](that: BiMap[J, K]) = that map this

  def |>[W](that: BiMap[V, W]) = this andThen that
  def <|[J](that: BiMap[J, K]) = this compose that

  override def |>[W](that: Bijection[V, W]) = ???
  override def <|[J](that: Bijection[J, K]) = ???


  override def toString = "{" + pairs.map { case (k, v) => s"$k ↔︎ $v" }.buildString(", ") + "}"
}

abstract class AbstractBiMap[K, V] extends AbstractMap[K, V] with BiMap[K, V]

