package poly.collection.impl.linkedlist

import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * A singly linked list.
 * Performance:
 *
 *  - Accessing by index: O(''n'')
 *  - Appending: amortized O(''n'')
 *  - Prepending: O(1)
 *  - Insertion at any index: O(''n'') (searching) + O(1) (insertion)
 *  - Removing at any index: O(''n'') (searching) + O(1) (removing)
 * @author Tongfei Chen
 */

class SinglyLinkedList[T, N >: Null <: SinglyLinkedNodeLike[T, N]] {

  var dummy: N = null

  private[poly] var len: Int = 0
  private[poly] var lastNode = dummy


  def length = len

  /**
   * Locates the ''i''th element in a singly linked list.
   * @param i Index
   * @return The previous node and the node that contains the ''i''-th element.
   */
  def locate(i: Int): (N, N) = {
    if (i == -1) return (dummy, dummy)
    if (i < 0 || i >= len) throw new IndexOutOfBoundsException
    var curr = dummy.next
    var prev: N = dummy
    var j = 0
    while (j < i) {
      prev = curr
      curr = curr.next
      j += 1
    }
    (prev, curr)
  }

  def appendInplace(n: N) = {
    lastNode.next = n
    lastNode = n
    len += 1
  }

  def prependInplace(n: N) = {
    n.next = dummy.next
    dummy.next = n
    len += 1
  }

  def apply(i: Int) = locate(i)._2.data

  /**
   * Sets the ''i''-th element of this doubly linked list to the specified value.
   * @param i Index
   * @param x The new value
   */
  def update(i: Int, x: T) = {
    val (_, node) = locate(i)
    node.data = x
  }

  def insertNodeAfter(i: N, x: N) = {
    x.next = i.next
    i.next = x
    len += 1
  }

  /**
   * Clears this singly linked list.
   */
  def clear() = {
    dummy.next = dummy
  }

  /**
   * Removes the ''i''-th element.
   * @param i Index
   */
  def deleteNodeAfter(i: N) = {
    i.next = i.next.next
    len -= 1
  }

  def entriesIterator: Iterator[N] = new Iterator[N] {
    private[this] var c = dummy
    def advance() = {
      if (c.next != dummy) {
        c = c.next
        true
      }
      else false
    }
    def current = c
  }

  def entries = Iterable.ofIterator(entriesIterator)

}
