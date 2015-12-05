package poly.collection.impl

import poly.collection._
import poly.collection.builder._
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
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
final class ResizableSeq[T]
  (private[this] var cap: Int = Settings.ArrayInitialSize) extends KeyMutableSeq[T] with DataMutableIndexedSeq[T]
{ self =>

  private[poly] var data: Array[AnyRef] = Array.ofDim[AnyRef](math.max(nextPowerOfTwo(cap), Settings.ArrayInitialSize))
  private[poly] var len: Int = 0

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
      System.arraycopy(data, 0, newData, 0, cap)
      data = newData
      cap = newCapacity
    }
  }

  def grow() = ensureCapacity(cap * 2)

  def fastApply(i: Int) = data(i).asInstanceOf[T]

  def capacity = cap

  def fastLength = len

  def update(i: Int, x: T) = data(i) = x.asInstanceOf[AnyRef]

  def clear() = len = 0

  def insertAt(i: Int, x: T) = {
    if (cap < len + 1) ensureCapacity(len + 1)
    Array.copy(data, i, data, i + 1, len - i)
    data(i) = x.asInstanceOf[AnyRef]
    len += 1
  }

  def deleteAt(i: Int): Unit = {
    Array.copy(data, i + 1, data, i, len - i - 1)
    len -= 1
  }

  def moveInplace(i: Int, j: Int, k: Int): Unit = {
    Array.copy(data, i, data, k, j - i)
  }

  def prependInplace(x: T) = insertAt(0, x)

  def appendInplace(x: T) = {
    if (cap < len + 1) ensureCapacity(len + 1)
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
    def +=(x: T) = a.appendInplace(x)
    def result = a
  }

}
