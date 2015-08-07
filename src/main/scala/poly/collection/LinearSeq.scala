package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait LinearSeq[+T] extends Seq[T] {

  def headNode: SeqNode[T]

  def length = {
    var node = headNode
    var n = 0
    while (node != null) {
      node = node.next
      n += 1
    }
    n
  }

  def apply(i: Int): T = {
    var node = headNode
    var j = 0
    while (j < i) {
      node = node.next
      j += 1
    }
    node.data
  }
}
