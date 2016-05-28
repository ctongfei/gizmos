package poly.collection.impl

import poly.collection._
import poly.collection.exception._
import poly.collection.impl.specialized._
import poly.collection.node._

/**
 * A binary tree stored in an array.
 * @author Tongfei Chen
 */
class ArrayBinaryTree[T] private(
  private[poly] val data: ResizableSeq[T],
  private[poly] val state: SpResizableArray[Boolean]
) extends AbstractIndexedBinaryTree[T] { self ⇒

  def fastApply(i: Int) = data(i)

  def update(i: Int, x: T) = {
    state(i) = true
    data(i) = x
  }

  def contains(i: Int) = {
    i >= 0 && i < data.fastLength && state(i)
  }

}

