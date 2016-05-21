package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.impl.hashtable._

/**
 * A hash set.
 * @author Tongfei Chen
 */
class HashSet[T: Hashing] private(val data: OpenHashTable[T, HashSet.Entry[T]]) extends KeyMutableSet[T] {

  import HashSet._

  def eqOnKeys = implicitly[Hashing[T]]

  def addInplace(x: T) = if (!contains(x)) data.addEntry(new Entry(x)) // TODO: calculate hash function only once?

  def removeInplace(x: T) = data.removeEntry(x)

  def contains(x: T) = data.locate(x) != null

  def clear() = data.clear()

  def keys = Iterable.ofIterator(data.entryIterator).map(_.key).withKnownSize(size)

  override def foreach[U](f: T => U) = data.foreachEntry(e => f(e.key))

  override def size: Int = data.size
}

object HashSet extends BuilderFactoryEv[HashSet, Hashing] {

  private[poly] class Entry[K](val key: K) extends OpenHashEntryLike[K, Entry[K]]

  implicit def newBuilder[T: Hashing]: Builder[T, HashSet[T]] = new Builder[T, HashSet[T]] {
    private[this] val ht = new OpenHashTable[T, Entry[T]]()
    private[this] val s = new HashSet[T](ht)
    def addInplace(x: T) = s.addInplace(x)
    def result = s
    override def sizeHint(n: Int) = ht.grow(n)
  }
}
