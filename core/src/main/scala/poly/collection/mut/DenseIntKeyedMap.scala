package poly.collection.mut

import cats.implicits._
import poly.collection._
import poly.collection.evidence._
import poly.collection.factory._
import poly.collection.impl._

import scala.language.higherKinds

/**
 * A special implementation of maps keyed by integers backed by an array.
 * This is efficient when most of the keys lies in the space from 0 to an integer ''n''
 * and most of the key space is actually used.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class DenseIntKeyedMap[T] private(
  private val data: ResizableArray[T]
) extends IntKeyedSortedMap[T] with KeyMutableMap[Int, T] {

  def keySet: SortedSet[Int] = new AbstractSortedSet[Int] {
    def keyOrder = Order[Int]
    def keys = Range(data.capacity).filter(i => data.get(i) != null)
    def contains(i: Int) = data.get(i) != null
  }

  def apply(x: Int): T = data(x)

  def update(x: Int, y: T): Unit = add_!(x, y)

  def ?(x: Int): Option[T] = Option(data get x).asInstanceOf[Option[T]]

  def add_!(x: Int, y: T): Unit = {
    if (x >= data.capacity)
      data.ensureCapacity(x + 1)
    data(x) = y
  }

  def clear_!(): Unit = data.fillWithNull()

  def remove_!(x: Int): Unit = data.set(x, null)

}

object DenseIntKeyedMap extends MapFactory[({type λ[K, V] = DenseIntKeyedMap[V]})#λ, IsInt] {

  def newMapBuilder[K: IsInt, V]: Builder[(K, V), DenseIntKeyedMap[V]] = new Builder[(Int, V), DenseIntKeyedMap[V]] {
    private[this] val data = new ResizableArray[V]()
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(kv: (Int, V)) = {
      val (k, v) = kv
      data(k) = v
    }
    def result = new DenseIntKeyedMap(data)
  }.asInstanceOf[Builder[(K, V), DenseIntKeyedMap[V]]] // typecast is safe, K =:= Int is known

}
