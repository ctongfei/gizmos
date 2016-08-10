package poly.collection.mut

import poly.collection._
import poly.collection.impl._
import poly.collection.impl.specialized._

/**
 * A binary tree stored in an array.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ArrayBinaryTree[T] private(
  private[poly] val data: ResizableSeq[T],
  private[poly] val state: SpResizableArray[Boolean]
) extends AbstractIndexedBinaryTree[T] { self =>

  def fastApply(i: Int) = data(i)

  def update(i: Int, x: T) = {
    state(i) = true
    data(i) = x
  }

  def contains(i: Int) = {
    i >= 0 && i < data.fastLength && state(i)
  }

}

