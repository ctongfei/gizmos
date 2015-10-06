package poly.collection.impl

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * An circular array.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class CircularArray[T](private val data: ResizableSeq[T]) {

  private[collection] var frontPtr = 0
  private[collection] var backPtr = data.fastLength

  private[this] def isFull = {
    val diff = backPtr - frontPtr
    diff == -1 || diff == (data.capacity - 1)
  }

  private[this] def grow() = {
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

  def isEmpty = frontPtr == backPtr

  def apply(i: Int) = data((frontPtr + i) % data.capacity)

  def length = {
    if (backPtr > frontPtr)
      backPtr - frontPtr
    else backPtr - frontPtr + data.capacity
  }

  def capacity = data.capacity

  def inplaceAppend(x: T) = {
    if (isFull) grow()
    data(backPtr) = x
    backPtr = (backPtr + 1) % data.capacity
  }

  def inplacePrepend(x: T) = {
    if (isFull) grow()
    val newFrontPtr = if (frontPtr > 0) frontPtr - 1 else data.capacity - 1
    data(newFrontPtr) = x
    frontPtr = newFrontPtr
  }

}
