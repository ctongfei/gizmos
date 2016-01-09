package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._

/**
  * An implementation of a binary min-heap.
  * The least element under the specific order will surface at the top of the heap.
  *
  * @author Tongfei Chen
  */
class BinaryHeap[T] private[poly](val data: ResizableSeq[T])(implicit val order: WeakOrder[T]) extends PriorityQueue[T] {

  import BinaryTree._

  @inline private[this] def smallerChildIndex(x: Int) = {
    val l = 2 * x + 1
    if (l < data.fastLength - 1 && data(l + 1) < data(l)) l + 1 else l
  }

  protected def siftUp(i: Int): Unit = {
    var p = i
    val t = data(p)
    while (p > 0 && t < data(parentIndex(p))) {
      data(p) = data(parentIndex(p))
      p = parentIndex(p)
    }
    data(p) = t
  }

  protected def siftDown(i: Int): Unit = {
    var p = i
    var c = smallerChildIndex(p)
    val t = data(p)
    while (c < data.length && t > data(c)) {
      data(p) = data(c)
      p = c
      c = smallerChildIndex(p)
    }
    data(p) = t
  }

  def push(x: T): Unit = {
    data.appendInplace(x)
    siftUp(data.fastLength - 1)
  }

  def pop(): T = {
    val front = data(0)
    data.swapInplace(0, data.length - 1)
    data.deleteAt(data.length - 1)
    if (data.length > 1) siftDown(0)
    front
  }

  def top = if (data.length <= 0) throw new QueueEmptyException else data(0)

  def elements: Iterable[T] = data

  override def size: Int = data.length

}



object BinaryHeap extends CollectionFactoryWithOrder[BinaryHeap] {

  implicit def newBuilder[T:WeakOrder]: Builder[T, BinaryHeap[T]] = new Builder[T, BinaryHeap[T]] {
    private[this] val data = new ResizableSeq[T]()
    def sizeHint(n: Int): Unit = data.ensureCapacity(n)
    def add(x: T): Unit = data.appendInplace(x)

    // heap building algorithm
    def result = {
      val h = new BinaryHeap[T](data)
      for (i ← Range(0, data.fastLength / 2).reverse)
        h.siftDown(i)
      h
    }
  }

}
