package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

/**
 * A binary heap backed priority queue.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryHeapPriorityQueue[T] private(private val heap: BinaryHeap[T]) extends PriorityQueue[T] {

  def order: WeakOrder[T] = heap.order

  def enqueue(x: T): Unit = heap.enqueue(x)

  def dequeue(): T = heap.dequeue()

  def front: T = heap.data(0)

  def size: Int = heap.size

}

object BinaryHeapPriorityQueue extends OrderedCollectionFactory[BinaryHeapPriorityQueue] {

  implicit def newBuilder[T:Tag:WeakOrder]: CollectionBuilder[T, BinaryHeapPriorityQueue] =
    new CollectionBuilder[T, BinaryHeapPriorityQueue] {
      val data = new ResizableArray[T]()
      def sizeHint(n: Int): Unit = data.ensureCapacity(n)
      def +=(x: T): Unit = data.append(x)
      def result: BinaryHeapPriorityQueue[T] = {
        val h = new BinaryHeap[T](data)
        for (i ← data.length / 2 - 1 to 0 by -1)
          h.siftDown(i)
        new BinaryHeapPriorityQueue[T](h)
      }
    }

}
