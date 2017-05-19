package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.impl.hashtable._
import poly.collection.typeclass._

/**
 * A hash set.
 * @author Tongfei Chen
 */
class HashSet[T: Hash] private(val data: OpenHashTable[T, HashSet.Entry[T]]) extends KeyMutableSet[T] {

  import HashSet._

  def keyEq = Hash[T]

  def add_!(x: T) = if (!contains(x)) data.addEntry(new Entry(x)) // TODO: calculate hash function only once?

  def remove_!(x: T) = data.removeEntry(x)

  def contains(x: T) = data.locate(x) != null

  def clear_!() = data.clear()

  def keys = Iterable.ofIterator(data.entryIterator).map(_.key).asIfSizeKnown(size)

  override def foreach[U](f: T => U) = data.foreachEntry(e => f(e.key))

  override def size = data.size
}

object HashSet extends SetFactory[HashSet, Hash] {

  private[poly] class Entry[K](val key: K) extends OpenHashEntryLike[K, Entry[K]]

  def newSetBuilder[T: Hash]: Builder[T, HashSet[T]] = new Builder[T, HashSet[T]] {
    private[this] val ht = new OpenHashTable[T, Entry[T]]()
    private[this] val s = new HashSet[T](ht)
    def add(x: T) = s.add_!(x)
    def result = s
    override def sizeHint(n: Int) = ht.grow(n)
  }
}
