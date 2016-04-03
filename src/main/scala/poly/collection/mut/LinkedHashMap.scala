package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl.hashtable._
import poly.collection.node._

/**
 * Represents a linked hash map.
 * When traversing through this hash map, the order of the key-value pairs
 * will retain the order under which they were inserted.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class LinkedHashMap[K: IntHashing, V] private(val data: OpenHashTable[K, LinkedHashMap.Entry[K, V]]) extends KeyMutableMap[K, V] {

  import LinkedHashMap._

  private[this] object dummy extends Entry[K, V](default[K], default[V], null, null) {
    override def data = throw new DummyNodeException
    override def isDummy = true
  }
  dummy.prev = dummy
  dummy.next = dummy


  def apply(k: K) = data.locate(k).value

  def ?(k: K) = {
    val e = data.locate(k)
    if (e == null) None else Some(e.value)
  }


  def add(x: K, y: V) = {
    val e = new Entry(x, y, dummy.prev, dummy)
    e.prev.next = e
    e.next.prev = e
    data.addEntry(e)
  }

  def clear() = {
    data.clear()
    dummy.prev = dummy
    dummy.next = dummy
  }

  def remove(x: K) = {
    val e = data.locate(x)
    if (e != null) {
      e.prev.next = e.next
      e.next.prev = e.prev
    }
    data.removeEntry(x)
  }

  def update(x: K, y: V) = {
    val e = data.locate(x)
    if (e != null) e.value = y
    else add(x, y)
  }

  def pairs: BiSeq[(K, V)] = BiSeq.ofDummyNode(dummy)

  override def keys = pairs map firstOfPair

  override def values = pairs map secondOfPair

  this.keySet

  def containsKey(x: K) = data.locate(x) != null

  def equivOnKey = implicitly[IntHashing[K]]
}

object LinkedHashMap extends MapFactoryWithIntHashing[LinkedHashMap] {

  private[poly] class Entry[K, V](val key: K, var value: V, var prev: Entry[K, V], var next: Entry[K, V])
    extends OpenHashEntryLike[K, Entry[K, V]] with BiSeqNode[(K, V)] {
    def data = (key, value)
    def isDummy = false
  }

  implicit def newBuilder[K: IntHashing, V]: Builder[(K, V), LinkedHashMap[K, V]] = new Builder[(K, V), LinkedHashMap[K, V]] {
    private[this] val m = new LinkedHashMap[K, V](new OpenHashTable[K, Entry[K, V]])
    def addInplace(x: (K, V)) = m.add(x)
    def result = m
    def sizeHint(n: Int) = m.data.grow(n)
  }

}
