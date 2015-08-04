package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ListSet[T] private(private val data: SinglyLinkedList[T]) extends MutableSet[T] {

  override def size = data.len

  def contains(x: T): Boolean = {
    var found = false
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data == x)
        return true
      c = c.next
    }
    false
  }

  def add(x: T) = {
    if (!contains(x)) data.inplacePrepend(x)
  }

  def remove(x: T) = {
    var p = data.dummy
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data == x) p.next = c.next
      p = c
      c = c.next
    }
  }

  def elements = data
}

//TODO:!!! change to SetFactory
object ListSet {
  def apply[T](xs: T*): ListSet[T] = {
    val l = new SinglyLinkedList[T]
    xs foreach l.inplaceAppend
    new ListSet[T](l)
  }
}