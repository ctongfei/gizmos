package poly.collection

import poly.algebra._

/**
 * Represents a bijective map whose key-value pairs can be iterated.
 * This is used to model one-to-one correspondence.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiMap[K, V] extends Map[K, V] with Bijection[K, V] with Relation[K, V] { self =>

  /** Returns the set of values in this map. */
  def valueSet: Set[V]

  /** Returns the equivalence relation on the value set of this bijective map. */
  def valueEq: Eq[V] = valueSet.keyEq

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
  def containsValue(v: V) = valueSet.containsKey(v)

  override def values = valueSet.keys

  def related(k: K, v: V) = valueEq.eq(self(k), v)

  // HELPER FUNCTIONS

  /** Returns the inverse map that maps values to keys. $LAZY */
  override def inverse: BiMap[V, K] = new AbstractBiMap[V, K] {
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

  def map[W](that: BiMap[V, W]): BiMap[K, W] = ??? //TODO

  override def map[W](that: Bijection[V, W]): BiMap[K, W] = ???
  override def contramap[J](that: Bijection[J, K]): BiMap[J, V] = ???

  def contramap[J](that: BiMap[J, K]) = that map this

  def andThen[W](that: BiMap[V, W]) = this map that
  def compose[J](that: BiMap[J, K]) = that map this

  def |>[W](that: BiMap[V, W]) = this andThen that
  def |>:[J](that: BiMap[J, K]) = this compose that

  override def |>[W](that: Bijection[V, W]) = ???
  override def |>:[J](that: Bijection[J, K]) = ???


  override def toString = "{" + pairs.map { case (k, v) => s"$k ↔︎ $v" }.buildString(", ") + "}"
}

abstract class AbstractBiMap[K, V] extends AbstractMap[K, V] with BiMap[K, V]

