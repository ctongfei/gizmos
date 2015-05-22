package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class DoublyLinkedListNode[T] (
  var data: T,
  private[poly] var prev: DoublyLinkedListNode[T] = null,
  private[poly] var next: DoublyLinkedListNode[T] = null
) extends Node[T] {

  def descendants = ListSeq(next)
  def ancestors = ListSeq(prev)

}
