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
class ArrayIntMap[T] private(
  private val data: ResizableArray[T],
  private val state: SpResizableArray[Boolean]
) extends KeyMutableMap[Int, T] {

  def equivOnKey = Equiv.default[Int]

  def apply(x: Int): T = data(x)

  def update(x: Int, y: T): Unit = {
    data(x) = y
    state(x) = true
  }

  def ?(x: Int): Option[T] = {
    if (state(x)) Some(data(x)) else None
  }

  def pairs: Iterable[(Int, T)] = ???

  override def size: Int = ???

  def containsKey(x: Int): Boolean = state(x)

  def add(x: Int, y: T): Unit = {
    state(x) = true
    data(x) = y
  }

  def clear(): Unit = FastLoop.ascending(0, data.capacity, 1) { i => state(i) = false }

  def remove(x: Int): Unit = state(x) = false

}
