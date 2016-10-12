package poly.collection

/**
 * Represents a bidirectional multimap.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiMultimap[K, V] extends KeyedLike[K, BiMultimap[K, V]] with Multimap[K, V] { self =>

  def valueSet: Set[V]

  def invert(v: V): Set[K]

  def containsValue(v: V) = valueSet contains v

  def values = valueSet.keys

  def valueEq = valueSet.keyEq

  override def filterKeys(f: K => Boolean): BiMultimap[K, V] = new BiMultimapT.KeyFiltered(self, f)

  def filterValues(f: V => Boolean): BiMultimap[K, V] = new BiMultimapT.ValueFiltered(self, f)

  override def inverse: BiMultimap[V, K] = new BiMultimapT.Inverse(self)

}

abstract class AbstractBiMultimap[K, V] extends BiMultimap[K, V]

private[poly] object BiMultimapT {

  class Inverse[K, V](self: BiMultimap[K, V]) extends AbstractBiMultimap[V, K] {
    def keySet = self.valueSet
    def valueSet = self.keySet
    def apply(v: V) = self invert v
    def invert(k: K) = self apply k
    override def inverse = self
  }

  class KeyFiltered[K, V](self: BiMultimap[K, V], f: K => Boolean) extends AbstractBiMultimap[K, V] {
    def valueSet = self.valueSet.filter(v => self.invert(v) exists f)
    def invert(v: V) = self.invert(v) filter f
    def keySet = self.keySet filter f
    def apply(k: K) = if (f(k)) self(k) else Set.empty[V](self.valueEq)
    override def pairs = self.pairs.filter(f compose first)
  }

  class ValueFiltered[K, V](self: BiMultimap[K, V], f: V => Boolean) extends AbstractBiMultimap[K, V] {
    def valueSet = self.valueSet filter f
    def invert(v: V) = if (f(v)) self.invert(v) else Set.empty[K](self.keyEq)
    def keySet = self.keySet.filter(k => self(k) exists f)
    def apply(k: K) = self(k) filter f
    override def pairs = self.pairs.filter(f compose second)
  }

}
