package poly.collection.mut

import poly.algebra._
import poly.algebra.conversion.FromJava._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromJava._
import poly.collection.factory._

/**
  * Represents a map backed by a red-black tree. This class is a wrapper of `java.util.TreeMap`.
  * @author Tongfei Chen
  */
class RedBlackTreeMap[K, V] private(private val data: java.util.TreeMap[K, V]) extends KeyMutableMap[K, V] with SortedMap[K, V] {

  def apply(k: K) = data get k

  def ?(k: K) = if (data containsKey k) Some(data get k) else None

  def add(x: K, y: V) = data.put(x, y)

  def clear() = data.clear()

  def remove(x: K) = data.remove(x)

  def pairs = new SortedIterable[(K, V)] {
    def order = orderOnKey contramap firstOfPair
    def newIterator = data.entrySet().elements.map(e => (e.getKey, e.getValue)).newIterator
  }

  def orderOnKey = data.comparator()

  def update(x: K, y: V) = data.put(x, y)

  override def size = data.size()

  def containsKey(x: K) = data.containsKey(x)
}

object RedBlackTreeMap extends SortedMapFactory[RedBlackTreeMap] {

  implicit def newBuilder[K, V](implicit K: WeakOrder[K]): Builder[(K, V), RedBlackTreeMap[K, V]] =
    new Builder[(K, V), RedBlackTreeMap[K, V]] {
      private[this] val data = new java.util.TreeMap[K, V](new java.util.Comparator[K] {
        def compare(a: K, b: K) = K.cmp(a, b)
      })
      def sizeHint(n: Int) = {}
      def result = new RedBlackTreeMap(data)
      def add(x: (K, V)) = data.put(x._1, x._2)
    }
}
