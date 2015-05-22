package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class ListSet[T] extends Set[T] {

  private[this] val data: DoublyLinkedList[T] = new DoublyLinkedList[T]

  override def size = data.length

  def contains[U >: T](x: U): Boolean = {
    var found = false
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data == x)
        return true
      c = c.next
    }
    false
  }
}
