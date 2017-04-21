package poly.collection.impl

import poly.collection._
import poly.macroutil._

/**
 * @author Tongfei Chen
 */
final class ResizableArray[T]
(private[this] var cap: Int = Settings.ArrayInitialSize) { self =>

  private[this] var data: Array[AnyRef] = Array.ofDim[AnyRef](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))

  def ensureCapacity(minCapacity: Int): Unit = {
    if (cap < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[AnyRef](newCapacity)
      System.arraycopy(data, 0, newData, 0, cap)
      data = newData
      cap = newCapacity
    }
  }

  def moveInplace(i: Int, j: Int, k: Int): Unit = {
    System.arraycopy(data, i, data, k, j - i)
  }


  def apply(i: Int) = data(i).asInstanceOf[T]

  /** Type-unsafe getter (can result in `null`). Use with care. */
  def get(i: Int): AnyRef = data(i)

  /** Type-unsafe setter (can set `null`) Use with care. */
  def set(i: Int, x: AnyRef) = {
    if (i >= capacity) ensureCapacity(i + 1)
    data(i) = x
  }

  def capacity = cap

  def update(i: Int, x: T) = {
    if (i >= capacity) ensureCapacity(i + 1)
    data(i) = x.asInstanceOf[AnyRef]
  }

  def fillInplace(x: T) = FastLoop.ascending(0, capacity, 1) { i =>
    data(i) = x.asInstanceOf[AnyRef]
  }

  def fillWithNull() = FastLoop.ascending(0, capacity, 1) { i =>
    data(i) = null
  }

}
