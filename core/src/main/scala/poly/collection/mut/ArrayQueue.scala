package poly.collection.mut

import poly.collection._
import poly.collection.evidence._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._

/**
 * An array-backed circular queue that supports amortized O(1) time for both pushing and popping.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ArrayQueue[T] private(private val data: ResizableArray[T]) extends Queue[T] {

  private var frontPtr = 0
  private var backPtr = 0

  private def isFull = {
    val diff = backPtr - frontPtr
    diff == -1 || diff == (data.capacity - 1)
  }

  private def grow() = {
    // extend the buffer by 2
    //       B F
    // [ 6 7 - 1 2 3 4 5 ]
    //   *****
    // becomes
    //         F             B
    // [ - - - 1 2 3 4 5 6 7 - - - - - - ]
    //                   *****
    val originalCapacity = data.capacity
    data.ensureCapacity(originalCapacity * 2)
    data.moveInplace(0, frontPtr, originalCapacity)
    backPtr += originalCapacity
  }

  override def isEmpty = frontPtr == backPtr

  override def size = {
    if (backPtr >= frontPtr)
      backPtr - frontPtr
    else backPtr - frontPtr + data.capacity
  }

  def elements = IndexedSeq.tabulate(size)(i => data((frontPtr + i) % data.capacity))

  def front = {
    if (isEmpty) throw new QueueEmptyException
    data(frontPtr)
  }

  def enqueue(x: T) = {
    if (isFull) grow()
    data(backPtr) = x
    backPtr = (backPtr + 1) % data.capacity
  }

  def dequeue(): T = {
    val x = front
    frontPtr = (frontPtr + 1) % data.capacity
    x
  }
}

object ArrayQueue extends SeqFactory[ArrayQueue] {

  def newSeqBuilder[T]: Builder[T, ArrayQueue[T]] = new Builder[T, ArrayQueue[T]] {
    private[this] val a = new ResizableArray[T]()
    private[this] var n = 0
    override def sizeHint(n: Int) = a.ensureCapacity(n)
    def add(x: T) = {
      a(n) = x
      n += 1
    }
    def result = {
      val q = new ArrayQueue[T](a)
      q.frontPtr = 0
      q.backPtr = n
      q
    }
  }

}
