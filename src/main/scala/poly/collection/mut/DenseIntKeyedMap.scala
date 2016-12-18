package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

import scala.language.higherKinds

/**
 * A special implementation of maps keyed by integers backed by an array.
 * Inserting a `null` in this collection is considered as nonexistent.
 * This is efficient when most of the keys lies in the space from 0 to an integer ''n''
 * and most of the key space is actually used.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class DenseIntKeyedMap[T] private(
  private val data: ResizableArray[T],
  private var n: Int = 0
) extends IntKeyedSortedMap[T] with KeyMutableMap[Int, T] {

  def keySet: SortedSet[Int] = Range(n).asSet.filter(i => data.get(i) != null)

  def apply(x: Int): T = data(x)

  def update(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
    }
    if (data.get(x) == null && y != null) n += 1
    data(x) = y
  }

  def ?(x: Int): Option[T] = Option(data get x).asInstanceOf[Option[T]]

  override def size: Int = n

  def add_!(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
    }
    data(x) = y
    if (y != null) n += 1
  }

  def clear_!(): Unit = data.fillWithNull()

  def remove_!(x: Int): Unit = {
    if (data.get(x) != null) n -= 1
    data.set(x, null)
  }

}

object DenseIntKeyedMap extends BuilderFactoryInt2[DenseIntKeyedMap] {

  implicit def newBuilder[V]: Builder[(Int, V), DenseIntKeyedMap[V]] = new Builder[(Int, V), DenseIntKeyedMap[V]] {
    private[this] val data = new ResizableArray[V]()
    private[this] var n = 0
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(kv: (Int, V)) = {
      val (k, v) = kv
      if (v != null) {
        data(k) = v
        n += 1
      }
    }
    def result = new DenseIntKeyedMap(data, n)
  }


}
