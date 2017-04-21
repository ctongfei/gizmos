package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.node._

/**
 * Represents a bidirectional list backed by a doubly-linked list.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class ListBidiSeq[T] private() extends AbstractBidiSeq[T] with KeyMutableSeq[T] {

  type Node = ListBidiSeq.Node[T]

  override val dummy: Node = new Node(default[T], dummy, dummy) { override def isDummy = true }

  private[poly] var len: Int = 0
  dummy.prev = dummy
  dummy.next = dummy

  override def length = len
  override def size = len
  override def sizeKnown = true

  def headNode = dummy.next

  def lastNode = dummy.prev

  /**
   * Locates the ''i''th element in a doubly linked list.
   *
   * @param i Index
   * @return The node that contains the ''i''-th element.
   */
  def locate(i: Int): Node = { //TODO: for i >= length / 2, find backwards for faster speed
    if (i < 0 || i >= len) throw new IndexOutOfBoundsException
    var curr = dummy.next
    var j = 0
    while (j < i) {
      curr = curr.next
      j += 1
    }
    curr
  }

  /**
   * Appends an element to the end of the doubly linked list.
   * @param x The element to be appended
   */
  def append_!(x: T) = {
    val node = new Node(x, dummy.prev, dummy)
    node.prev.next = node
    node.next.prev = node
    len += 1
  }

  def prepend_!(x: T) = {
    val node = new Node(x, dummy, dummy.next)
    node.prev.next = node
    node.next.prev = node
    len += 1
  }

  override def apply(i: Int) = locate(i).data

  def update(i: Int, x: T) = {
    val node = locate(i)
    node.data = x
  }

  def insert_!(i: Int, x: T) = {
    val p = locate(i)
    val node = new Node(x, p.prev, p)
    node.prev.next = node
    node.next.prev = node
    len += 1
  }

  def clear_!() = {
    dummy.prev = dummy
    dummy.next = dummy
    // leave the other nodes to GC!
  }

  def delete_!(i: Int) = {
    val p = locate(i)
    p.prev.next = p.next
    p.next.prev = p.prev
    len -= 1
  }

  def reverse_!(): Unit = {
    var p = dummy.prev
    var c = dummy
    var n = dummy.next
    do {
      c.prev = n
      c.next = p
      p = c
      c = n
      n = n.next
    } while (c != dummy)
  }

  override def map_!(f: T => T): Unit = {
    var c = dummy.next
    while (c != dummy) {
      c.data = f(c.data)
      c = c.next
    }
  }
}

object ListBidiSeq extends SeqFactory[ListBidiSeq] {

  /**
   * Type of the internal node of a linked list.
   *
   * @param data Data held in this node
   * @param prev The previous node
   * @param next The next node
   */
  private[poly] class Node[T](var data: T, var prev: Node[T], var next: Node[T]) extends BidiSeqNode[T] {
    def isDummy = false
  }

  def newSeqBuilder[T]: Builder[T, ListBidiSeq[T]] = new Builder[T, ListBidiSeq[T]] {
    private[this] val l = new ListBidiSeq[T]()
    def result = l
    def add(x: T) = l.append_!(x)
  }
}
