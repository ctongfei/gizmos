package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.node._

/**
 * A sequence backed by a linked list.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class ListSeq[T] private(private val data: SinglyLinkedList[T]) extends StructureMutableSeq[T] {

  def headNode: SeqNode[T] = data.dummy.next

  def apply(i: Int) = data.apply(i)

  def length = data.length

  def inplaceAppend(x: T) = data.inplaceAppend(x)

  def inplacePrepend(x: T) = data.inplacePrepend(x)

  def update(i: Int, x: T) = data.update(i, x)

  def insertAt(i: Int, x: T) = data.insertAt(i, x)

  def clear() = data.clear()

  def deleteAt(i: Int) = data.deleteAt(i)

  def inplaceMap(f: T => T) = ???

  def inplaceReverse() = ???

  override def newEnumerator = new Enumerator[T] {
    var node = data.dummy

    def advance(): Boolean = {
      if (node.next.isDummy) false
      else {
        node = node.next
        true
      }
    }

    def current: T = {
      if (node.isDummy) throw new EnumeratorPositionException
      node.data
    }
  }
}

object ListSeq extends SeqFactory[ListSeq] {

  implicit def newBuilder[T]: Builder[T, ListSeq[T]] = new Builder[T, ListSeq[T]] {
    val a = new SinglyLinkedList[T]()
    def sizeHint(n: Int) = {}
    def +=(x: T) = a inplaceAppend x
    def result = new ListSeq[T](a)
  }
}
