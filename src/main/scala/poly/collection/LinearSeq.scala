package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait LinearSeq[+T] extends Seq[T] { self =>

  def headNode: SeqNode[T]

  def length = {
    var node = headNode
    var n = 0
    while (node.notDummy) {
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

  override def foreach[V](f: T => V) = {
    var node = headNode
    while (!node.isDummy) {
      f(node.data)
      node = node.next
    }
  }

  // HELPER FUNCTIONS

  override def foldLeft[U](z: U)(f: (U, T) => U): U = {
    ???
  }

  override def foldRight[U](z: U)(f: (T, U) => U): U = {
    if (headNode.isDummy) z
    else f(head, tail.foldRight(z)(f))
  }

}

abstract class AbstractLinearSeq[+T] extends LinearSeq[T]

