package poly.collection.mut

import poly.collection._
import poly.collection.typeclass._
import poly.collection.factory._
import poly.collection.immut._

/**
 * A stack backed by a singly-linked list.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ListStack[T] private(private var data: List[T]) extends Queue[T] {

  private[this] var s = 0

  override def size = s

  def enqueue(x: T): Unit = {
    s += 1
    data = x :: data
  }

  def front: T = data.head

  def dequeue(): T = {
    val t = data.head
    data = data.tail
    s -= 1
    t
  }

  def elements = data

}

object ListStack extends SeqFactory[ListStack] {

  def newSeqBuilder[T]: Builder[T, ListStack[T]] = List.newBuilder[T] map { l => new ListStack(l) }

}
