package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.impl._

/**
 * A set backed by a linked list.
 * @author Tongfei Chen
 */
class ListSet[T] private(private val data: SinglyLinkedList[T])(implicit val equivOnKey: Equiv[T]) extends MutableSet[T] {

  override def size = data.len

  def contains(x: T): Boolean = {
    var found = false
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data =~= x)
        return true
      c = c.next
    }
    false
  }

  def add(x: T) = {
    if (!contains(x)) data.prependInplace(x)
  }

  def remove(x: T) = {
    var p = data.dummy
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data =~= x) p.next = c.next
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
    xs foreach l.appendInplace
    new ListSet[T](l)
  }
}