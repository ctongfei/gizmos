package poly.collection

/**
 * Represents a bidirectional multimap.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiMultimap[K, V] extends KeyedLike[K, BiMultimap[K, V]] with Multimap[K, V] { self =>

  def invert(v: V): Set[K]

  def containsValue(v: V): Boolean

  def values: Iterable[V]

  override def valueSet = new AbstractSet[V] {
    def eqOnKeys = self.eqOnValues
    def keys = self.values
    def contains(v: V) = self.containsValue(v)
  }

  override def filterKeys(f: K => Boolean): BiMultimap[K, V] = ???

  def filterValues(f: V => Boolean): BiMultimap[K, V] = ???

  override def inverse: BiMultimap[V, K] = new BiMultimapT.Inverse(self)

}

abstract class AbstractBiMultimap[K, V] extends BiMultimap[K, V]

private[poly] object BiMultimapT {

  class Inverse[K, V](self: BiMultimap[K, V]) extends AbstractBiMultimap[V, K] {
    def eqOnKeys = self.eqOnValues
    def eqOnValues = self.eqOnKeys
    def keys = self.values
    def values = self.keys
    def containsKey(v: V) = self containsValue v
    def containsValue(k: K) = self containsKey k
    def apply(v: V) = self invert v
    def invert(k: K) = self apply k
    override def inverse = self
  }

}
