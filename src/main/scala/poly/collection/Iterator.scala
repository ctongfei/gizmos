package poly.collection

import poly.algebra.specgroup._
import poly.collection.exception._

/**
 * Represents iterators that support an iteration over a collection that can be
 * paused and resumed by the consumer.
 *
 * This is the Poly-collection version of the Java/Scala `Iterator`. The main difference
 * is their abstraction of supported methods:
 *
 * <ul>
 *  <li> Java/Scala `Iterator`: { `hasNext`, `next` } </li>
 *  <li> Poly-collection `Iterator`: { `current`, `advance` } </li>
 *  <li> C++ iterators : { `*`, `++` } </li>
 *  <li> C# `Enumerator`: { `Current`, `MoveNext` } </li>
 * </ul>
 *
 *  Poly-collection adopts the C++/C# style of iterators instead of the Java/Scala style
 *  for ease of implementing lazy search algorithms and lazy stream operations.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Iterator[@sp +T] { self =>

  /** Returns the current element of this iterator. This method should be side-effect free. */
  def current: T

  /**
   * Advances the iterator to the next element.
   * @return Whether the iterator successfully advanced to the next element.
   */
  def advance(): Boolean

  /**
   * Alternative abstraction: Optionally reads the iterator by one step.
   * @return If the next element exists, returns it and advances itself; otherwise it returns [[None]].
   */
  def read(): Option[T] = {
    if (self.advance()) Some(self.current)
    else None
  }

  override def toString = try {
    s"Current = ${current.toString}"
  } catch {
    case _: Exception => "<invalid position>"
  }

}

object Iterator {

  /** An empty iterator. */
  object empty extends Iterator[Nothing] {
    def advance() = false
    def current = throw new InvalidIteratorPositionException
  }
}
