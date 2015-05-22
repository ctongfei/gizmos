package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SinglyLinkedListNode[T] (
  var data: T,
  private[poly] var next: SinglyLinkedListNode[T] = null
) extends ForwardNode[T] {

  def descendants = ListSeq(next)

}
