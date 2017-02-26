package poly.collection.immut

import poly.collection.factory._

/**
 * Represents an immutable stack.
 * @author Tongfei Chen
 */
class ImStack[T] private(val elements: List[T]) {

  def push(x: T) = new ImStack(x :: elements)

  def top = elements.head

  def pop = (elements.head, new ImStack(elements.tail))

}

object ImStack extends SeqFactory[ImStack] {
  def newSeqBuilder[T] = List.newBuilder[T] map { l => new ImStack(l) }
}
