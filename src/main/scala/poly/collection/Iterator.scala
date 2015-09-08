package poly.collection

import poly.collection.exception._
import poly.collection.mut._

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
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Iterator[+T] { self =>

  /** Returns the current element of this iterator. */
  def current: T

  /**
   * Advances the iterator to the next element.
   * @return Whether the iterator successfully advanced to the next element.
   */
  def advance(): Boolean

  override def toString = "Current = " + this.current.toString

}

object Iterator {
  object empty extends Iterator[Nothing] {
    def advance() = false
    def current = throw new NoSuchElementException
  }

  def single[T](x: T): Iterator[T] = new AbstractIterator[T] {
    var curr: T = _
    var first = false
    def advance() = {
      if (first) {
        curr = x
        first = false
        true
      } else false
    }
    def current = curr
  }

  def iterate[T](s: T)(next: T => T): Iterator[T] = new AbstractIterator[T] {
    private[this] var curr: T = _
    private[this] var first = true
    def advance() = {
      if (first) {
        first = false
        curr = s
      } else curr = next(curr)
      true
    }
    def current = curr
  }

}

abstract class AbstractIterator[+T] extends Iterator[T]
