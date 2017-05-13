package poly.collection.mut

import poly.collection._
import poly.collection.evidence._
import poly.collection.factory._
import poly.collection.impl._

/**
 * A special implementation of map keyed by integer pairs backed by a 2-D array.
 * This is efficient when most of the keys lies in the space [0, n) × [0, n) and
 * most of the key space is actually used.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class DenseIntPairKeyedMap[T] private(
  private val data: ResizableTable[T]
) extends KeySortedMap[(Int, Int), T] with KeyMutableMap[(Int, Int), T] {

  def keySet: SortedSet[(Int, Int)] = new AbstractSortedSet[(Int, Int)] {
    def keyOrder = new Order[(Int, Int)] {
      override def compare(p: (Int, Int), q: (Int, Int)) = {
        if (p._1 != q._1) p._1 - q._1
        else p._2 - q._2
      }
    }
    def keys = Range(data.rowCapacity).product(Range(data.colCapacity)).filter { case (i, j) => data.get(i, j) != null }.asIfSorted(keyOrder)
    def contains(x: (Int, Int)) = data.get(x._1, x._2) != null
  }

  def apply(k: (Int, Int)) = data(k._1, k._2)

  def add_!(i: Int, j: Int, e: T) = {
    if (i >= data.rowCapacity || j >= data.colCapacity)
      data.ensureRowColCapacity(i + 1, j + 1)
    data(i, j) = e
  }

  def add_!(k: (Int, Int), v: T) = add_!(k._1, k._2, v)

  def remove_!(k: (Int, Int)) = data(k._1, k._2) = null.asInstanceOf[T]

  def clear_!() = data.fillWithNull()

  def update(i: Int, j: Int, e: T) = add_!(i, j, e)

  def update(k: (Int, Int), v: T) = update(k._1, k._2, v)

  def ?(k: (Int, Int)) = Option(data get (k._1, k._2)).asInstanceOf[Option[T]]

}

object DenseIntPairKeyedMap extends MapFactory[({type λ[α, β] = DenseIntPairKeyedMap[β]})#λ, IsIntPair] {
  def newMapBuilder[K: IsIntPair, V]: Builder[(K, V), DenseIntPairKeyedMap[V]] = new Builder[((Int, Int), V), DenseIntPairKeyedMap[V]] {
    private[this] val data = new ResizableTable[V]()
    def add(x: ((Int, Int), V)) = {
      val ((i, j), v) = x
      data(i, j) = v
    }
    def result() = new DenseIntPairKeyedMap[V](data)
  }.asInstanceOf[Builder[(K, V), DenseIntPairKeyedMap[V]]] // typecast is safe
}
