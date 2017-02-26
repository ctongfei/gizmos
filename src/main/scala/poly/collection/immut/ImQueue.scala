package poly.collection.immut

import poly.collection.factory._

/**
 * Represents an immutable queue.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class ImQueue[T] private(val in: List[T], val out: List[T]) {

  def push(x: T) = new ImQueue(x :: in, out)

  def pop = {
    if (out.notEmpty) (out.head, new ImQueue(in, out.tail))
    else {
      val r = in.reverse to List
      (r.head, new ImQueue(List.Empty, r.tail))
    }
  }

}

object ImQueue extends SeqFactory[ImQueue] {
  def newSeqBuilder[T] = List.newBuilder[T] map { l => new ImQueue(List.Empty, l) }
}
