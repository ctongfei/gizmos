package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl.hashtable._
import poly.collection.node._
import poly.collection.typeclass._

/**
 * Represents a linked hash set.
 * When traversing through this hash set, the order of the elements will retain the order under which they were inserted.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class LinkedHashSet[T: Hashing] private(val data: OpenHashTable[T, LinkedHashSet.Entry[T]]) extends KeyMutableSet[T] {

  import LinkedHashSet._

  private[this] object dummy extends Entry[T](default[T], null, null) {
    override def data = throw new DummyNodeException
    override def isDummy = true
  }
  dummy.prev = dummy
  dummy.next = dummy

  def clear_!() = data.clear()

  def add_!(x: T) = {
    val e = new Entry(x, dummy.prev, dummy)
    e.prev.next = e
    e.next.prev = e
    data.addEntry(e)
  }

  def remove_!(x: T) = {
    val e = data.locate(x)
    e.prev.next = e.next
    e.next.prev = e.prev
    data.removeEntry(x)
  }

  def keyEq = Hashing[T]

  def keys: BidiSeq[T] = BidiSeq.ofDummyNode(dummy)

  override def elements = keys

  def contains(x: T) = data.locate(x) != null
}

object LinkedHashSet extends Factory1[Id, LinkedHashSet, Hashing] {

  private[poly] class Entry[K](val key: K, var prev: Entry[K], var next: Entry[K])
    extends OpenHashEntryLike[K, Entry[K]] with BidiSeqNode[K] {
    def data = key
    def isDummy = false
  }

  implicit def newBuilder[T: Hashing]: Builder[T, LinkedHashSet[T]] = new Builder[T, LinkedHashSet[T]] {
    private[this] val s = new LinkedHashSet[T](new OpenHashTable[T, Entry[T]]())
    def add(x: T) = s.add_!(x)
    def result = s
    override def sizeHint(n: Int) = s.data.grow(n)
  }

}