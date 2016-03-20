package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.impl._
import poly.collection.impl.specialized._
import poly.macroutil._

/**
 * A special implementation of maps keyed by integers backed by an array.
 * This is efficient when the keys are contiguous from 0 to a not-too-large integer ''n''
 * and most of the key space is actually used.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class DenseIntKeyedMap[T] private(
  private val data: ResizableArray[T],
  private val state: SpResizableArray[Boolean],
  private var n: Int = 0
) extends KeyMutableMap[Int, T] {

  def equivOnKey = poly.algebra.std.IntStructure

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

