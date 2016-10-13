package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
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
  private val data: ResizableArray[T],
  private val state: BitSet,
  private var n: Int = 0
) extends IntKeyedSortedMap[T] with KeyMutableMap[Int, T] {

  def keySet: SortedSet[Int] = state

  def apply(x: Int): T = data(x)

  def update(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
    }
    data(x) = y
    if (!state(x)) n += 1
    state += x
  }

  def ?(x: Int): Option[T] = if (state(x)) Some(data(x)) else None

  override def pairs = state.createMap(i => data(i)).pairs

  override def size: Int = n

  def add_!(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
    }
    state += x
    data(x) = y
    n += 1
  }

  def clear_!(): Unit = state.clear_!()

  def remove_!(x: Int): Unit = {
    if (state(x)) n -= 1
    state -= x
  }

}

object DenseIntKeyedMap extends BuilderFactoryIntA[DenseIntKeyedMap] {

  implicit def newBuilder[V]: Builder[(Int, V), DenseIntKeyedMap[V]] = new Builder[(Int, V), DenseIntKeyedMap[V]] {
    private[this] val data = new ResizableArray[V]()
    private[this] val state = BitSet()
    private[this] var n = 0
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(x: (Int, V)) = {
      state += x._1
      data(x._1) = x._2
      n += 1
    }
    def result = new DenseIntKeyedMap(data, state, n)
  }


}