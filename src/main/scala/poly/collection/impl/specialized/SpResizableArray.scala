package poly.collection.impl.specialized

import poly.algebra.specgroup._
import poly.collection._

import scala.reflect._

/**
 * A specialized version for resizable arrays ([[poly.collection.impl.ResizableArray]]).
 * @author Tongfei Chen
 */
final class SpResizableArray[@sp(fdib) T: ClassTag]
(private[this] var cap: Int = Settings.ArrayInitialSize) { self =>

  private[this] var data: Array[T] = Array.ofDim[T](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))

  def ensureCapacity(minCapacity: Int): Unit = {
    if (cap < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[T](newCapacity)
      System.arraycopy(data, 0, newData, 0, cap)
      data = newData
      cap = newCapacity
    }
  }

  def apply(i: Int) = data(i)

  def capacity = cap

  def update(i: Int, x: T) = {
    if (i >= cap) ensureCapacity(i + 1)
    data(i) = x
  }

}
