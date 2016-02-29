package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.exception._
import poly.collection.impl._
import poly.collection.impl.specialized._
import poly.util.fastloop._

/**
 * @author Tongfei Chen
 */
class SparseArrayIntMap[T] private(
  private val data: ResizableArray[T],
  private val state: SpResizableArray[Boolean],
  private var n: Int = 0
) extends KeyMutableMap[Int, T] {

  def equivOnKey = Equiv.default[Int]

  def apply(x: Int): T = data(x)

  def update(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
      state.ensureCapacity(x + 1)
    }
    data(x) = y
    if (!state(x)) n += 1
    state(x) = true
  }

  def ?(x: Int): Option[T] = {
    if (state(x)) Some(data(x)) else None
  }

  def pairs: Iterable[(Int, T)] = Range(data.capacity).filter(i => state(i)).map(i => (i, data(i)))

  override def size: Int = n

  def containsKey(x: Int): Boolean = state(x)

  def add(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
      state.ensureCapacity(x + 1)
    }
    state(x) = true
    data(x) = y
    n += 1
  }

  def clear(): Unit = {
    FastLoop.ascending(0, data.capacity, 1) { i => state(i) = false }
    n = 0
  }

  def remove(x: Int): Unit = {
    if (state(x)) n -= 1
    state(x) = false
  }

}
