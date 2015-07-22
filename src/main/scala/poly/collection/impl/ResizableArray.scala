package poly.collection.impl

import poly.collection._
import poly.collection.conversion._
import poly.collection.factory._
import poly.collection.node._
import poly.util.specgroup._
import scala.reflect._

/**
 * A resizable array. This serves as the implementation container of `ArrayX` classes.
 * Performance:
 *
 *  - Accessing by index: O(1)
 *  - Appending: amortized O(1)
 *  - Prepending: O(''n'')
 *  - Insertion at any index: O(''n'')
 *  - Deletion at any index: O(''n'')
 *
 * This class serves as the basic building block for a series of structures in Poly-collection.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
final class ResizableArray[T]
  (private[this] var cap: Int = Settings.ArrayInitialSize) extends StructureMutableIndexedSeq[T]
{ self =>

  private[this] var data: Array[AnyRef] = Array.ofDim[AnyRef](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))
  private[this] var len: Int = 0

  private[poly] def getData = data // exposed for math libraries

  class Node(val i: Int) extends BiNode[T] {
    def data = self.data(i).asInstanceOf[T]
    def pred = {
      if (i < 0) throw new IllegalArgumentException
      else if (i == 0) Seq()
      else Seq(new Node(i - 1))
    }
    def succ = {
      if (i >= self.length) throw new IllegalArgumentException
      else if (i == self.length - 1) Seq()
      else Seq(new Node(i + 1))
    }
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    if (cap < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[AnyRef](newCapacity)
      Array.copy(data, 0, newData, 0, cap) // copy all for ArrayQueue
      data = newData
      cap = newCapacity
    }
  }

  def grow() = ensureCapacity(cap * 2)

  def apply(i: Int) = data(i).asInstanceOf[T]

  def capacity = cap

  def length = len

  def update(i: Int, x: T) = data(i) = x.asInstanceOf[AnyRef]

  def clear() = len = 0

  def insertAt(i: Int, x: T) = {
    ensureCapacity(len + 1)
    Array.copy(data, i, data, i + 1, len - i)
    data(i) = x.asInstanceOf[AnyRef]
    len += 1
  }

  def deleteAt(i: Int): Unit = {
    Array.copy(data, i + 1, data, i, len - i - 1)
    len -= 1
  }

  def move(i: Int, j: Int, k: Int): Unit = {
    Array.copy(data, i, data, k, j - i)
  }

  def prepend(x: T) = insertAt(0, x)

  def append(x: T) = {
    ensureCapacity(len + 1)
    data(len) = x.asInstanceOf[AnyRef]
    len += 1
  }

  def appendUnchecked(x: T) = {
    data(len) = x.asInstanceOf[AnyRef]
    len += 1
  }
}

object ResizableArray extends SeqFactory[ResizableArray] {
  def newBuilder[T]: Builder[T, ResizableArray[T]] = new Builder[T, ResizableArray[T]] {
    val a = new ResizableArray[T]()
    def sizeHint(n: Int) = a.ensureCapacity(n)
    def +=(x: T) = a.append(x)
    def result = a
  }

}
