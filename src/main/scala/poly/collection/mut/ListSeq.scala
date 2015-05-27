package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._

/**
 * A sequence backed by a linked list.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ListSeq[T] private() extends MutSeq[T] {

  private var data: DoublyLinkedList[T] = null

  def length = data.length

  def append(x: T) = data.append(x)

  def prepend(x: T) = data.prepend(x)

  def update(i: Int, x: T) = data.update(i, x)

  def inplaceReverse() = ???

  def insertAt(i: Int, x: T) = data.insert(i, x)

  def clear() = data.clear()

  def deleteAt(i: Int) = data.remove(i)

  def inplaceMap(f: T => T) = ???

  def swap(i: Int, j: Int) = ???

  def apply(i: Int) = data.apply(i)

  def enumerator = new Enumerator[T] {
    var node = data.dummy

    def advance(): Boolean = {
      if (node.next eq data.dummy) {
        node = data.dummy
        false
      }
      else {
        node = node.next
        true
      }
    }

    def current: T = {
      if (node eq data.dummy) throw new EnumeratorPositionException
      node.data
    }
  }
}

object ListSeq extends SeqFactory[ListSeq] {

  implicit def newBuilder[T]: CollectionBuilder[T, ListSeq] = new CollectionBuilder[T, ListSeq] {
    val a = new DoublyLinkedList[T]()
    def sizeHint(n: Int) = {}
    def +=(x: T) = a.append(x)
    def result = {
      val res = new ListSeq[T]
      res.data = a
      res
    }
  }
}
