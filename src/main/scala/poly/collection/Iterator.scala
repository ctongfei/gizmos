package poly.collection

import poly.collection.exception._

/**
 * Basic trait for iterators that supports an iteration over a collection that can be
 * paused and resumed.
 *
 * This is the Poly-collection version of the Java/Scala `Iterator`. The main difference
 * is their abstraction of supported methods:
 *
 *  - Java/Scala `Iterator`: { `hasNext`, `next` }
 *  - Poly-collection `Iterator`: { `current`, `advance` }
 *  - C++ iterators : { `*`, `++` }
 *  - C# `Enumerator`: { `Current`, `MoveNext` }
 *
 *  Poly-collection adopts the C++/C# style of iterators instead of the Java/Scala style
 *  for ease of implementing lazy search algorithms and lazy stream operations.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Iterator[+T] { self =>

  /** Returns the current element of this iterator. This method should be side-effect free. */
  def current: T

  /**
   * Advances the iterator to the next element.
   * @return Whether the iterator successfully advanced to the next element.
   */
  def advance(): Boolean

  /**
   * Alternative abstraction: Optionally reads the iterator by one step.
   * @return If the next element exists, returns it and advances itself; otherwise [[None]].
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
  object empty extends Iterator[Nothing] {
    def advance() = false
    def current = throw new InvalidIteratorPositionException
  }
}
