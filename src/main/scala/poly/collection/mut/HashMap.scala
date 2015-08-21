package poly.collection.mut

import poly.collection._
import poly.collection.conversion._
import poly.collection.factory._
import scala.collection.JavaConverters._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class HashMap[K, V] private(private val data: java.util.HashMap[K, V]) extends KeyMutableMap[K, V] {

  def apply(x: K): V = data.get(x)

  def containsKey(x: K): Boolean = data.containsKey(x)

  def update(x: K, y: V): Unit = data.put(x, y)

  def ?(x: K): Option[V] = Option(data.get(x))

  def add(x: K, y: V): Unit = data.put(x, y)

  def remove(x: K): Unit = data.remove(x)

  def clear(): Unit = data.clear()

  def size = data.size

  def pairs: Enumerable[(K, V)] = data.asScala

}

object HashMap extends MapFactory[HashMap] {
  implicit def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = new Builder[(K, V), HashMap[K, V]] {
    private val data = new java.util.HashMap[K, V]()
    def sizeHint(n: Int) = {}
    def +=(x: (K, V)) = data.put(x._1, x._2)
    def result = new HashMap[K, V](data)
  }
}
