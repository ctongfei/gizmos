package poly.collection.mut

import poly.algebra.conversion._
import poly.algebra.hkt.ops._
import poly.collection._
import poly.collection.conversion.Java._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class RBTreeMap[K, V] private(val data: java.util.TreeMap[K, V]) extends KeyMutableMap[K, V] with SortedMap[K, V] {

  def apply(k: K) = data get k

  def ?(k: K) = if (data containsKey k) Some(data get k) else None

  def add(x: K, y: V) = data.put(x, y)

  def clear() = data.clear()

  def remove(x: K) = data.remove(x)

  def pairs = new SortedIterable[(K, V)] {
    implicit def orderOnValue = orderOnKey contramap first
    def newIterator = data.entrySet().map(e => (e.getKey, e.getValue)).newIterator
  }

  def orderOnKey = data.comparator()

  def update(x: K, y: V) = data.put(x, y)

  def size = data.size()

  def containsKey(x: K) = data.containsKey(x)
}
