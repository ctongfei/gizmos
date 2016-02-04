package poly.collection.mut

import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
class CustomizableStack[S[α] <: KeyMutableSeq[α], T] private(val inner: S[T]) extends Queue[T] {

  def elements: Iterable[T] = inner

  def push(x: T) = inner.appendInplace(x)

  override def pushAll(xs: Traversable[T]) = {
    val reversed = xs.reverse
    reversed foreach push
  }

  def top = inner.last

  def pop = ???
}
