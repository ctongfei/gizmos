package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.macroutil._

/**
 * An implementation of a binary min-heap.
 * The least element under the specific order will always surface to the top of the heap.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class BinaryHeap[T] private(private val data: ResizableSeq[T])(implicit val elementOrder: Order[T]) extends PriorityQueue[T] {

  import BinaryTree._

  private def smallerChildIndex(x: Int) = {
    val l = 2 * x + 1
    if (l < data.fastLength - 1 && data(l + 1) < data(l)) l + 1 else l
  }

  private def siftUp(i: Int): Unit = {
    var p = i
    val t = data(p)
    while (p > 0 && t < data(parentIndex(p))) {
      data(p) = data(parentIndex(p))
      p = parentIndex(p)
    }
    data(p) = t
  }

  private def siftDown(i: Int): Unit = {
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

  def push(x: T) = {
    data.append_!(x)
    siftUp(data.fastLength - 1)
  }

  def pop() = {
    val front = data(0)
    data.swap_!(0, data.length - 1)
    data.delete_!(data.length - 1)
    if (data.length > 1) siftDown(0)
    front
  }

  def top = if (data.length <= 0) throw new QueueEmptyException else data(0)

  def elements: Iterable[T] = data

  override def size = data.length

}

object BinaryHeap extends Factory1[Id, BinaryHeap, Order] {

  implicit def newBuilder[T: Order]: Builder[T, BinaryHeap[T]] = new Builder[T, BinaryHeap[T]] {
    private[this] val data = new ResizableSeq[T]()
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(x: T) = data.append_!(x)

    // heap building algorithm
    def result = {
      val h = new BinaryHeap[T](data)
      FastLoop.descending(data.fastLength / 2, -1, -1) { i =>
        h.siftDown(i)
      }
      h
    }
  }

}
