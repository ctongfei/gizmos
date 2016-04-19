package poly.collection.mut

import poly.algebra._
import poly.algebra.ops._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._

/**
 * A set backed by a linked list.
 * @author Tongfei Chen
 */
class ListSet[T] private(private val data: SinglyLinkedList[T])(implicit val equivOnKey: Equiv[T]) extends KeyMutableSet[T] {

  override def size = data.len

  def contains(x: T): Boolean = {
    var found = false
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data === x)
        return true
      c = c.next
    }
    false
  }


  def clear() = data.clear()

  def add(x: T) = {
    if (!contains(x)) data.prependInplace(x)
  }

  def remove(x: T) = {
    var p = data.dummy
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (c.data === x) p.next = c.next
      p = c
      c = c.next
    }
  }

  def keys: Seq[T] = data
}

object ListSet extends SetFactory[ListSet] {

  implicit def newBuilder[K: Equiv]: Builder[K, ListSet[K]] = new Builder[K, ListSet[K]] {
    private[this] val s = new ListSet(new SinglyLinkedList[K]())
    def addInplace(x: K) = s add x
    def result = s
    def sizeHint(n: Int) = {}
  }

}