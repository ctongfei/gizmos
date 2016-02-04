package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import poly.collection.factory._
import poly.collection.impl.hashtable._
import scala.collection.JavaConverters._

/**
 * @author Tongfei Chen
 */
class HashMap[K: IntHashing, V] private(private val data: OpenHashTable[K, HashMap.Entry[K, V]]) extends KeyMutableMap[K, V] {

  import HashMap._

  def equivOnKey = implicitly[IntHashing[K]]

  def apply(k: K): V = data.locate(k).value

  def containsKey(k: K): Boolean = data.locate(k) != null

  def update(k: K, v: V): Unit = data.locate(k).value = v

  def ?(k: K): Option[V] = {
    val e = data.locate(k)
    if (e != null) Some(e.value) else None
  }

  def add(k: K, v: V): Unit = data.addEntry(new Entry(k, v))

  def remove(k: K): Unit = data.removeEntry(k)

  def clear(): Unit = data.clear()

  override def size = data.size

  def pairs: Iterable[(K, V)] = Iterable.ofIterator(data.entryIterator).map(e => e.key → e.value)

}

object HashMap extends MapFactoryWithIntHashing[HashMap] {

  private[poly] class Entry[K, V](val key: K, var value: V) extends HashEntryLike[K, Entry[K, V]]

  implicit def newBuilder[K: IntHashing, V]: Builder[(K, V), HashMap[K, V]] = new Builder[(K, V), HashMap[K, V]] {
    private val data = new OpenHashTable[K, Entry[K, V]]()
    def sizeHint(n: Int) = data.grow(n)
    def add(x: (K, V)) = data.addEntry(new Entry(x._1, x._2))
    def result = new HashMap[K, V](data)
  }
}
