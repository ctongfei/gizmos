package poly.collection.mut

import poly.collection._
import poly.collection.conversion._
import scala.collection.JavaConverters._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class HashMap[K, V] private(private val data: java.util.HashMap[K, V]) extends StructureMutableMap[K, V] {

  def add(x: K, y: V): Unit = data.put(x, y)

  def clear(): Unit = data.clear()

  def remove(x: K): Unit = data.remove(x)

  def update(x: K, y: V): Unit = data.put(x, y)

  def applyOption(x: K): Option[V] = Option(data.get(x))

  def pairs: Enumerable[(K, V)] = data.asScala

  def apply(x: K): V = data.get(x)

  def contains(x: K): Boolean = data.containsKey(x)

}
