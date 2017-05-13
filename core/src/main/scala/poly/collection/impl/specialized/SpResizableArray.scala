package poly.collection.impl.specialized

import poly.collection.specgroup._
import poly.collection._
import poly.macroutil._

import scala.reflect._

/**
 * A specialized version for resizable arrays ([[poly.collection.impl.ResizableArray]]).
 * @author Tongfei Chen
 * @since 0.1.0
 */
class SpResizableArray[@sp(Float, Double, Int, Long, Boolean) T: ClassTag]
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

  def fillInplace(x: T) = FastLoop.ascending(0, capacity, 1) { i =>
    data(i) = x
  }

}

// hand-specialized versions
class IntResizableArray(private[this] var cap: Int = Settings.ArrayInitialSize) extends SpResizableArray[Int](cap)
class LongResizableArray(private[this] var cap: Int = Settings.ArrayInitialSize) extends SpResizableArray[Long](cap)
class FloatResizableArray(private[this] var cap: Int = Settings.ArrayInitialSize) extends SpResizableArray[Float](cap)
class DoubleResizableArray(private[this] var cap: Int = Settings.ArrayInitialSize) extends SpResizableArray[Double](cap)
class BooleanResizableArray(private[this] var cap: Int = Settings.ArrayInitialSize) extends SpResizableArray[Boolean](cap)
