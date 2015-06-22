package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * Represents a node that has only one predecessor node.
 * @since 0.1.0
 */
trait SinglePredNode[+T] extends BackwardNode[T] { self =>
  def parent: SinglePredNode[T]
  def pred: Enumerable[SinglePredNode[T]] = if (parent eq null) ListSeq() else ListSeq(parent)
  def map[U](f: T => U): SinglePredNode[U] = new SinglePredNode[U] {
    def parent = self.parent.map(f)
    def data = f(self.data)
  }
}
