package poly.collection.impl

import poly.collection._
import poly.collection.conversion._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.util.specgroup._
import scala.reflect._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
final class ResizableArray[T]
(private[this] var cap: Int = Settings.ArrayInitialSize) { self =>

  private[this] var data: Array[AnyRef] = Array.ofDim[AnyRef](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))

  private[poly] def getData = data // exposed for math libraries

  def ensureCapacity(minCapacity: Int): Unit = {
    if (cap < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[AnyRef](newCapacity)
      Array.copy(data, 0, newData, 0, cap) // copy all for ArrayQueue
      data = newData
      cap = newCapacity
    }
  }

  def apply(i: Int) = data(i).asInstanceOf[T]

  def capacity = cap

  def update(i: Int, x: T) = {
    if (i >= capacity) ensureCapacity(i + 1)
    data(i) = x.asInstanceOf[AnyRef]
  }

}
