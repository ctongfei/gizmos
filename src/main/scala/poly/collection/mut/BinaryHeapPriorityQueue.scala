package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * A binary heap backed priority queue.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryHeapPriorityQueue[T] private(private val heap: BinaryHeap[T]) extends PriorityQueue[T] {

  val order: WeakOrder[T] = heap.order

  def push(x: T): Unit = heap.push(x)

  def pop(): T = heap.pop()

  def top: T = heap.data(0)

  def size: Int = heap.size

}

object BinaryHeapPriorityQueue extends CollectionFactoryWithOrder[BinaryHeapPriorityQueue] {

  implicit def newBuilder[T:WeakOrder]: Builder[T, BinaryHeapPriorityQueue[T]] = new Builder[T, BinaryHeapPriorityQueue[T]] {
    private[this] val data = new ResizableSeq[T]()
    def sizeHint(n: Int): Unit = data.ensureCapacity(n)
    def +=(x: T): Unit = data.inplaceAppend(x)

    // heap building algorithm
    def result: BinaryHeapPriorityQueue[T] = {
      val h = new BinaryHeap[T](data)
      for (i ← data.length / 2 - 1 to 0 by -1)
        h.siftDown(i)
      new BinaryHeapPriorityQueue[T](h)
    }
  }

}
