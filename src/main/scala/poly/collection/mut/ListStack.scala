package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ListStack[@specialized(Int, Double) T] private(private var data: ForwardLinkedList[T]) extends Queue[T] {

  def size = data.len

  def push(x: T): Unit = data.prepend(x)

  def top: T = data.dummy.next.data

  def pop(): T = {
    val t = top
    data.remove(0)
    t
  }

}

object ListStack extends CollectionFactory[ListStack] {

  implicit def newBuilder[T]: CollectionBuilder[T, ListStack] = new CollectionBuilder[T, ListStack] {
    var data: ForwardLinkedList[T] = null
    def sizeHint(n: Int) = {}
    def +=(x: T) = data.prepend(x)
    def result = new ListStack[T](data)
  }

}
