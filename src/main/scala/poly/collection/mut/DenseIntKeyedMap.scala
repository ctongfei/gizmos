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
  private val state: BitSet,
  private var n: Int = 0
) extends IntKeyedSortedMap[T] with KeyMutableMap[Int, T] {

  implicit def orderOnKeys = poly.algebra.std.IntStructure

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

  def pairs = state.createMapBy(i ⇒ data(i)).pairs

  override def size: Int = n

  def containsKey(x: Int): Boolean = state(x)

  def addInplace(x: Int, y: T): Unit = {
    if (x >= data.capacity) {
      data.ensureCapacity(x + 1)
    }
    state += x
    data(x) = y
    n += 1
  }

  def clear(): Unit = state.clear()


  def removeInplace(x: Int): Unit = {
    if (state(x)) n -= 1
    state -= x
  }

}

object DenseIntKeyedMap {

}