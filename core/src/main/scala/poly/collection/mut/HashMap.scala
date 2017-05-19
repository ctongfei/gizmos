package poly.collection.mut

import poly.collection._
import poly.collection.typeclass._
import poly.collection.factory._
import poly.collection.impl.hashtable._

/**
 * Represents a map backed by an open hash table.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class HashMap[K: Hash, V] private(private val data: OpenHashTable[K, HashMap.Entry[K, V]]) extends KeyMutableMap[K, V] {

  import HashMap._

  def keySet: Set[K] = new AbstractSet[K] {
    def keyEq = Hash[K]
    def contains(k: K) = data.locate(k) != null
    def keys = data.entries.map(_.key)
  }

  def apply(k: K): V = data.locate(k).value

  def update(k: K, v: V) = data.locate(k).value = v

  def ?(k: K): Option[V] = {
    val e = data.locate(k)
    if (e != null) Some(e.value) else None
  }

  def add_!(k: K, v: V): Unit = {
    val e = data.locate(k)
    if (e != null) e.value = v
    else data.addEntry(new Entry(k, v))
  }

  def remove_!(k: K): Unit = data.removeEntry(k)

  def clear_!(): Unit = data.clear()

  override def size = data.size

  override def pairs = data.entries.map(e => e.key -> e.value).asIfSizeKnown(size)

}

object HashMap extends MapFactory[HashMap, Hash] {

  private[poly] class Entry[K, V](val key: K, var value: V) extends OpenHashEntryLike[K, Entry[K, V]]

  implicit def newMapBuilder[K: Hash, V]: Builder[(K, V), HashMap[K, V]] = new Builder[(K, V), HashMap[K, V]] {
    private[this] val ht = new OpenHashTable[K, Entry[K, V]]()
    private[this] val m = new HashMap(ht)
    override def sizeHint(n: Int) = ht.grow(n)
    def add(x: (K, V)) = m.add_!(x)
    def result = m
  }
}
