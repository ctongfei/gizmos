package poly.collection.impl

import poly.collection._
import poly.collection.conversion._
import poly.collection.factory._
import poly.collection.mut._
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
final class ResizableSeq[T]
  (private[this] var cap: Int = Settings.ArrayInitialSize) extends KeyMutableIndexedSeq[T]
{ self =>

  private[this] var data: Array[AnyRef] = Array.ofDim[AnyRef](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))
  private[this] var len: Int = 0

  private[poly] def getData = data // exposed for math libraries

  case class Node(i: Int) extends BiSeqNode[T] {
    override val isDummy = i < 0 || i >= len
    def data = self.data(i).asInstanceOf[T]
    def prev = new Node(i - 1)
    def next = new Node(i + 1)
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

  def inplaceMove(i: Int, j: Int, k: Int): Unit = {
    Array.copy(data, i, data, k, j - i)
  }

  def inplacePrepend(x: T) = insertAt(0, x)

  def inplaceAppend(x: T) = {
    ensureCapacity(len + 1)
    data(len) = x.asInstanceOf[AnyRef]
    len += 1
  }

  def appendUnchecked(x: T) = {
    data(len) = x.asInstanceOf[AnyRef]
    len += 1
  }
}

object ResizableSeq extends SeqFactory[ResizableSeq] {
  def newBuilder[T]: Builder[T, ResizableSeq[T]] = new Builder[T, ResizableSeq[T]] {
    val a = new ResizableSeq[T]()
    def sizeHint(n: Int) = a.ensureCapacity(n)
    def +=(x: T) = a.inplaceAppend(x)
    def result = a
  }

}
