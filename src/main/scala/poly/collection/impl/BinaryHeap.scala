package poly.collection.impl

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.exception._
import poly.collection.tree._

/**
 * An implementation of a binary min-heap.
 * The least element under the specific order will surface at the top of the heap.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryHeap[T](val data: ResizableArray[T])(implicit O: WeakOrder[T]) extends PriorityQueue[T] {

  /** Retrieves the order on the elements in this binary heap. */
  val order = O

  @inline private[this] def smallerChildIndex(x: Int) = {
    val l = 2 * x + 1
    if (l < data.length - 1 && data(l + 1) < data(l)) l + 1 else l
  }

  def siftUp(i: Int): Unit = {
    var p = i
    val t = data(p)
    while (p > 0 && t < data(BinaryTree.parentIndex(p))) {
      data(p) = data(BinaryTree.parentIndex(p))
      p = BinaryTree.parentIndex(p)
    }
    data(p) = t
  }

  def siftDown(i: Int): Unit = {
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
    data.append(x)
    siftUp(data.length - 1)
  }

  def pop(): T = {
    val front = data(0)
    data.swap(0, data.length - 1)
    data.deleteAt(data.length - 1)
    if (data.length > 1) siftDown(0)
    front
  }

  def top = if (size <= 0) throw new NoSuchElementException else data(0)

  def size: Int = data.length

}

