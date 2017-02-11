package poly.collection

import poly.algebra.specgroup._
import poly.collection.exception._
import poly.collection.immut._
import poly.macroutil._

import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.annotation.{unspecialized => unsp}

/**
 * Represents iterators that support an iteration over a collection that can be
 * paused and resumed by the consumer.
 *
 * This is the Poly-collection version of the Java/Scala [[java.util.Iterator]]/[[scala.collection.Iterator]].
 * The main difference is their abstraction of supported methods:
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
 *  [[Builder]]s are the categorical duals (read vs. write) of [[Iterator]]s.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
// Specializes the types where a specialized iterator type (including InputStream / Reader) exists in Java 8
trait Iterator[@sp(Int, Long, Double, Char, Byte) +T] { self =>

  /** Returns the current element of this iterator. This method should be side-effect free. */
  def current: T

  /**
   * Advances the iterator to the next element. This method is where side-effects should happen.
   * @return Whether the iterator successfully advanced to the next element.
   */
  def advance(): Boolean

  /**
   * Alternative abstraction: Optionally reads the iterator by one step.
   * @return If the next element exists, returns it and advances itself; otherwise it returns [[None]].
   */
  @unsp def read(): Option[T] = {
    if (self.advance()) Some(self.current)
    else None
  }

  def readToArray(a: Array[T @uv], off: Int, len: Int): Unit = {
    FastLoop.ascending(off, off + len, 1) { i =>
      if (!self.advance()) return
      a(i) = self.current
    }
  }

  def readToArray(a: Array[T @uv]): Unit = readToArray(a, 0, a.length)

  /** Runs this iterator with the given callback function. */
  def run[V](f: T => V) = {
    while (self.advance()) f(self.current)
  }

  /** Depletes all data from this iterator (source) to the given builder (sink). */
  def >>>[R](builder: Builder[T, R]) = {
    run(builder.add)
  }

  @unsp private[poly] def asLazyList: LazyList[T] = {
    if (self.advance()) LazyList.Cons(self.current, self.asLazyList)
    else LazyList.Empty
  }

  @unsp override def toString = try { s"⋯ $current ⋯" } catch {
    case _: Exception => "<invalid position>"
  }

}

object Iterator {

  /** An empty iterator. */
  object Empty extends Iterator[Nothing] {
    def advance() = false
    def current = throw new InvalidIteratorPositionException
  }
}

abstract class AbstractIterator[@sp(Int, Long, Double, Char, Byte) +T] extends Iterator[T]
