package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.exception._
import poly.collection.impl._
import poly.util.fastloop._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ArrayIntMap[T] private(
  private val data: ResizableArray[Option[T]]
) extends KeyMutableMap[Int, T] {

  def equivOnKey = Equiv.default[Int]

  def apply(x: Int): T = data(x).get

  def update(x: Int, y: T): Unit = data(x) = Some(y)

  def ?(x: Int): Option[T] = data(x)

  def pairs: Iterable[(Int, T)] = ???

  def size: Int = ???

  def containsKey(x: Int): Boolean = data(x).isDefined

  def add(x: Int, y: T): Unit = ???

  def clear(): Unit = for (i ← Range(data.capacity)) data(i) = None

  def remove(x: Int): Unit = data(x) = None

}
