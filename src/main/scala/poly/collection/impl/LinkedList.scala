package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * A doubly linked list.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedList[T] {

  type Node = LinkedList.Node[T]

  private[poly] val dummy = new Node(default[T])
  private[poly] var length: Int = 0
  dummy.prev = dummy
  dummy.next = dummy


  /**
   * Locates the ''i''th element in a doubly linked list.
   * @param i Index
   * @return The node that contains the ''i''-th element.
   */
  def locate(i: Int): Node = { //TODO: for i >= length / 2, find backwards for faster speed
    if (i < 0 | i >= length) throw new IndexOutOfBoundsException
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
  def append(x: T) = {
    val node = new Node(x, dummy.prev, dummy)
    node.prev.next = node
    node.next.prev = node
    length += 1
  }

  /**
   * Prepends an element to the start of the doubly linked list.
   * @param x The element to be prepended.
   */
  def prepend(x: T) = {
    val node = new Node(x, dummy, dummy.next)
    node.prev.next = node
    node.next.prev = node
    length += 1
  }

  /**
   * Gets the ''i''-th element.
   * @param i Index
   * @return The ''i''-th element.
   */
  def apply(i: Int) = locate(i).data

  /**
   * Sets the ''i''-th element of this doubly linked list to the specified value.
   * @param i Index
   * @param x The new value
   */
  def update(i: Int, x: T) = {
    val node = locate(i)
    node.data = x
  }

  /**
   * Inserts an element at the ''i''-th position.
   * @param i Index
   * @param x New element
   */
  def insert(i: Int, x: T) = {
    val p = locate(i)
    val node = new Node(x, p.prev, p)
    node.prev.next = node
    node.next.prev = node
    length += 1
  }

  /**
   * Clears this doubly linked list.
   */
  def clear() = {
    dummy.prev = dummy
    dummy.next = dummy
  }

  /**
   * Removes the ''i''-th element.
   * @param i Index
   */
  def remove(i: Int) = {
    val p = locate(i)
    p.prev.next = p.next
    p.next.prev = p.prev
    length -= 1
  }

}

object LinkedList {
  class Node[T] (
    var data: T,
    private[poly] var prev: Node[T] = null,
    private[poly] var next: Node[T] = null
  ) extends BidiNode[T] {

    def descendants = ListSeq(next)
    def ancestors = ListSeq(prev)

  }

}