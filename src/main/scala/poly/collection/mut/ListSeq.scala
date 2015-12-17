package poly.collection.mut

import poly.collection._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.node._

/**
 * A sequence backed by a linked list.
 *
 * @author Tongfei Chen
 */
class ListSeq[T] private(private val data: SinglyLinkedList[T]) extends AbstractSeq[T] with KeyMutableSeq[T] with HasKnownSize {

  def dummy: SeqNode[T] = data.dummy

  override def apply(i: Int) = data.apply(i)

  override def length = data.length

  def appendInplace(x: T) = data.appendInplace(x)

  def prependInplace(x: T) = data.prependInplace(x)

  def update(i: Int, x: T) = data.update(i, x)

  def insertAt(i: Int, x: T) = data.insertAt(i, x)

  def clear() = data.clear()

  def deleteAt(i: Int) = data.deleteAt(i)

  override def mapInplace(f: T => T) = {
    var n = data.dummy.next
    while (n.notDummy) {
      n.data = f(n.data)
      n = n.next
    }
  }

  def inplaceReverse() = ???

  override def newIterator = new Iterator[T] {
    var node = data.dummy

    def advance(): Boolean = {
      if (node.next.isDummy) false
      else {
        node = node.next
        true
      }
    }

    def current: T = {
      if (node.isDummy) throw new DummyNodeException
      node.data
    }
  }
}

object ListSeq extends SeqFactory[ListSeq] {

  implicit def newBuilder[T]: Builder[T, ListSeq[T]] = new Builder[T, ListSeq[T]] {
    val a = new SinglyLinkedList[T]()
    def sizeHint(n: Int) = {}
    def add(x: T) = a appendInplace x
    def result = new ListSeq[T](a)
  }
}
