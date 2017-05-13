package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._

/**
 * An array-backed mutable stack.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ArrayStack[T] private(private var data: ResizableSeq[T]) extends Queue[T] {

  def elements = data.reverse

  override def size = data.fastLength

  def enqueue(x: T): Unit = data.append_!(x)

  override def enqueueAll(xs: Traversable[T]) = {
    xs.reverse foreach enqueue // retain the node visiting sequence for DFS
  }

  def front: T = {
    if (isEmpty) throw new QueueEmptyException
    data(data.fastLength - 1)
  }

  def dequeue(): T = {
    val x = front
    data.delete_!(data.fastLength - 1)
    x
  }

}

object ArrayStack extends SeqFactory[ArrayStack] {

  def newSeqBuilder[T]: Builder[T, ArrayStack[T]] = new Builder[T, ArrayStack[T]] {
    var data = new ResizableSeq[T]()
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(x: T) = data.append_!(x)
    def result = new ArrayStack(data)
  }

}
