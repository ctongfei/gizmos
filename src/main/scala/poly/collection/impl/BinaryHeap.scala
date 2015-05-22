package poly.collection.impl

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.factory._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryHeap[T] private(val data: ResizableArray[T])(implicit O: WeakOrder[T]) {

  /** Retrieves the order on the elements in this binary heap. */
  val order = O

  @inline private[this] def parent(x: Int) = (x - 1) / 2
  @inline private[this] def smallerChild(x: Int) = {
    val l = 2 * x + 1
    if (l < data.length - 1 && data(l + 1) < data(l)) l + 1 else l
  }

  def siftUp(i: Int): Unit = {
    var p = i
    val t = data(p)
    while (p > 0 && t < data(parent(p))) {
      data(p) = data(parent(p))
      p = parent(p)
    }
    data(p) = t
  }

  def siftDown(i: Int): Unit = {
    var p = i
    var c = smallerChild(p)
    val t = data(p)
    while (c < data.length && t > data(c)) {
      data(p) = data(c)
      p = c
      c = smallerChild(p)
    }
    data(p) = t
  }

  def enqueue(x: T): Unit = {
    data.append(x)
    siftUp(data.length - 1)
  }

  def dequeue(): T = {
    val front = data(0)
    data.swap(0, data.length - 1)
    data.deleteAt(data.length - 1)
    if (data.length > 1) siftDown(0)
    front
  }

  def size: Int = data.length

}

