package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class ListSet[T] private(private val data: SinglyLinkedList[T]) extends Set[T] {

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
    if (!contains(x)) data.prepend(x)
  }


}
