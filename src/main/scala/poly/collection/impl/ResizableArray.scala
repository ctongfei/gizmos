package poly.collection.impl

import poly.collection._

/**
 * A resizable array. This serves as the implementation container of `ArrayX` classes.
 * Performance:
 *
 *  - Accessing by index: O(1)
 *  - Appending: amortized O(1)
 *  - Prepending: O(''n'')
 *  - Insertion at any index: O(''n'')
 *  - Removing at any index: O(''n'')
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ResizableArray[T: Tag](var capacity: Int = Settings.ArrayInitialSize) extends MutIndexedSeq[T] {

  private[this] var data: Array[T] = Array.ofDim[T](math.max(nextPowerOfTwo(capacity), Settings.ArrayInitialSize))
  private var len: Int = 0

  private[poly] def getData = data // exposed for math libraries

  def ensureCapacity(minCapacity: Int): Unit = {
    if (capacity < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[T](newCapacity)
      Array.copy(data, 0, newData, 0, capacity) // copy all for ArrayQueue
      data = newData
      capacity = newCapacity
    }
  }

  def grow() = ensureCapacity(capacity * 2)

  def apply(i: Int) = data(i)

  def length = len

  def update(i: Int, x: T) = data(i) = x

  def clear() = len = 0

  def insertAt(i: Int, x: T) = {
    ensureCapacity(len + 1)
    Array.copy(data, i, data, i + 1, len - i)
    data(i) = x
    len += 1
  }

  def deleteAt(i: Int): Unit = {
    Array.copy(data, i + 1, data, i, len - i)
    len -= 1
  }

  def move(i: Int, j: Int, k: Int): Unit = {
    Array.copy(data, i, data, k, j - i)
  }

  def prepend(x: T) = insertAt(0, x)

  def append(x: T) = {
    ensureCapacity(len + 1)
    data(len) = x
    len += 1
  }

  def appendUnchecked(x: T) = {
    data(len) = x
    len += 1
  }





}
