package poly.collection

import poly.collection.exception._
import poly.util.specgroup._

/**
 * Basic trait for enumerators that supports an iteration over a collection that can be
 * paused and resumed.
 *
 * This is the Poly-collection version of the Java/Scala `Iterator`. The main difference
 * is their abstraction of supported methods:
 *
 *  - Java/Scala `Iterable`: { `hasNext`, `next` }
 *  - Poly-collection `Enumerator`: { `current`, `advance` }
 *  - C++ iterators : { `*`, `++` }
 *  - C# `Enumerator`: { `Current`, `MoveNext` }
 *
 *  Poly-collection adopts the C++/C# style of iterators instead of the Java/Scala style
 *  for ease of implementing lazy search algorithms and lazy stream operations.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Enumerator[+T] extends Traversable[T] { self =>

  /** Returns the current element of this enumeration. */
  def current: T

  /**
   * Advances the enumerator to the next element.
   * @return Whether the enumerator successfully advanced to the next element.
   */
  def advance(): Boolean

  def foreach[U](f: T => U): Unit = {
    while (self.advance())
      f(self.current)
  }

  override def map[U](f: T => U): Enumerator[U] = new Enumerator[U] {
    def current = f(self.current)
    def advance() = self.advance()
  }

  def flatMap[U](f: T => Enumerator[U]): Enumerator[U] = new Enumerator[U] {
    private var e: Enumerator[U] = Enumerator.empty
    def current = e.current
    def advance(): Boolean = {
      if (e.advance()) true
      else {
        while (self.advance()) {
          e = f(self.current)
          if (e.advance()) return true
        }
        false
      }
    }
  }

  override def filter(f: T => Boolean): Enumerator[T] = new Enumerator[T] {
    def current: T = self.current
    def advance(): Boolean = {
      do {
        val hasNext = self.advance()
        if (!hasNext) return false
      } while (!f(self.current))
      true
    }
  }

  override def filterNot(f: T => Boolean): Enumerator[T] = filter(e => !f(e))

  def concat[U >: T](that: Enumerator[U]): Enumerator[U] = new Enumerator[U] {
    private[this] var e: Enumerator[U] = self
    def advance() = {
      if (e.advance()) true else {
        e = that
        e.advance()
      }
    }
    def current = e.current
  }

  override def tail = { self.advance(); self }

  override def take(n: Int): Enumerator[T] = new Enumerator[T] {
    private[this] var remaining = n
    def advance() = remaining > 0 && { remaining -= 1; self.advance() }
    def current = self.current
  }

  /**
   * Advances this enumerator past the first ''n'' elements.
   * @param n The number of elements to be dropped
   * @return
   */
  override def drop(n: Int): Enumerator[T] = {
    var i = 0
    while (i < n && advance()) i += 1
    this
  }

  override def slice(i: Int, j: Int) = self.drop(i).take(j - i)


  /**
   * Returns a collection formed from this collection and another iterable collection by combining
   * corresponding elements in pairs. For example, [1, 2, 3] zip [-1, -2, -3] yields [(1, -1), (2, -2), (3, -3)].
   * @param that Another enumerable collection
   * @return Zipped sequence
   * @since 0.1.0
   */
  def zip[U](that: Enumerator[U]): Enumerator[(T, U)] = new Enumerator[(T, U)] {
    def advance(): Boolean = self.advance() && that.advance()
    def current: (T, U) = (self.current, that.current)
  }

  /**
   * Returns the interleave sequence of two sequences. For example, [1, 2, 3] interleave [-1, -2, -3]
   * yields [1, -1, 2, -2, 3, -3].
   * @param that Another enumerable sequence
   * @return Interleave sequence
   * @since 0.1.0
   */
  def interleave[U >: T](that: Enumerator[U]): Enumerator[U] = new Enumerator[U] {
    var firstSeq = true
    def advance() = {
      firstSeq = !firstSeq
      if (!firstSeq) this.advance() else that.advance()
    }
    def current = if (firstSeq) this.current else that.current
  }

  override def toString = "Current = " + this.current.toString

}

object Enumerator {
  object empty extends Enumerator[Nothing] {
    def advance() = false
    def current = throw new NoSuchElementException
  }

  def iterate[T](s: T)(next: T => T): Enumerator[T] = new Enumerator[T] {
    private[this] var curr: T = _
    private[this] var first = true
    def advance() = {
      if (first) first = false else curr = next(curr)
      true
    }
    def current = curr
  }

}

