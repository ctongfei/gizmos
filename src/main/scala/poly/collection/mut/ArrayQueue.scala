package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._

/**
 * An array-backed circular queue that supports amortized O(1) time for both insertion and deletion.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ArrayQueue[@specialized(Int, Double) T] private(private val data: ResizableArray[T]) extends Cue[T] {

  private var frontPtr = 0
  private var backPtr = data.length

  private[this] def isFull = {
    val diff = backPtr - frontPtr
    diff == -1 || diff == (data.capacity - 1)
  }

  override def isEmpty = frontPtr == backPtr

  def front = {
    if (isEmpty) throw new QueueEmptyException
    data(frontPtr)
  }

  def size = {
    if (backPtr > frontPtr)
      backPtr - frontPtr
    else backPtr - frontPtr + data.capacity
  }

  def enqueue(x: T) = {
    if (isFull) { // extend the buffer by 2
      //       R F
      // [ 6 7 - 1 2 3 4 5 ]
      // becomes
      //         F             R
      // [ - - - 1 2 3 4 5 6 7 - - - - - - ]
      val originalCapacity = data.capacity
      data.ensureCapacity(originalCapacity * 2)
      data.move(0, frontPtr, originalCapacity)
      backPtr += originalCapacity
    }
    data(backPtr) = x
    backPtr = (backPtr + 1) % data.capacity
  }

  def dequeue(): T = {
    val x = front
    frontPtr = (frontPtr + 1) % data.capacity
    x
  }

}

object ArrayQueue extends TaggedCollectionFactory[ArrayQueue] {

  implicit def newBuilder[T: Tag]: CollectionBuilder[T, ArrayQueue] = new CollectionBuilder[T, ArrayQueue] {
    var a: ResizableArray[T] = new ResizableArray[T]()
    def sizeHint(n: Int) = a.ensureCapacity(n)
    def +=(x: T) = a.append(x)
    def result = new ArrayQueue[T](a)
  }

}
