package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import scala.reflect._

/**
 * An array-backed circular queue that supports amortized O(1) time for both insertion and deletion.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ArrayQueue[@specialized(Int, Double) T] private(private val data: CircularArray[T]) extends Queue[T] {

  def top = {
    if (data.isEmpty) throw new QueueEmptyException
    data(0)
  }

  def size = data.length

  def push(x: T) = data.append(x)

  def pop(): T = {
    val x = top
    data.frontPtr = (data.frontPtr + 1) % data.capacity
    x
  }

}

object ArrayQueue extends TaggedCollectionFactory[ArrayQueue] {

  implicit def newBuilder[T: ClassTag]: CollectionBuilder[T, ArrayQueue] = new CollectionBuilder[T, ArrayQueue] {
    var a: ResizableArray[T] = new ResizableArray[T]()
    def sizeHint(n: Int) = a.ensureCapacity(n)
    def +=(x: T) = a.append(x)
    def result = new ArrayQueue[T](new CircularArray[T](a))
  }

}
