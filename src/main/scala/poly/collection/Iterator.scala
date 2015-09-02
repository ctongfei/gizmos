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

  def foreach[U](f: T => U): Unit = while (self.advance()) f(self.current)

  def map[U](f: T => U): Iterator[U] = new AbstractIterator[U] {
    def current = f(self.current)
    def advance() = self.advance()
  }

  def flatMap[U](f: T => Iterator[U]): Iterator[U] = new AbstractIterator[U] {
    private var e: Iterator[U] = Iterator.empty
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

  def filter(f: T => Boolean): Iterator[T] = new AbstractIterator[T] {
    def current: T = self.current
    def advance(): Boolean = {
      do {
        val hasNext = self.advance()
        if (!hasNext) return false
      } while (!f(self.current))
      true
    }
  }

  def filterNot(f: T => Boolean): Iterator[T] = filter(e => !f(e))

  def concat[U >: T](that: Iterator[U]): Iterator[U] = new AbstractIterator[U] {
    private[this] var e: Iterator[U] = self
    def advance() = {
      if (e.advance()) true else {
        e = that
        e.advance()
      }
    }
    def current = e.current
  }

  def prepend[U >: T](u: U): Iterator[U] = new AbstractIterator[U] {
    private[this] var first = true
    private[this] var curr: U = _
    def advance() = if (first) {
      curr = u
      first = false
      true
    } else {
      val r = self.advance()
      curr = self.current
      r
    }
    def current = curr
  }

  def append[U >: T](u: U): Iterator[U] = new AbstractIterator[U] {
    private[this] var last = false
    def advance() = {
      if (last) false
      else {
        val r = self.advance()
        if (!r) last = true
        true
      }
    }
    def current = if (!last) self.current else u
  }

  def tail = { self.advance(); self }

  def take(n: Int): Iterator[T] = new AbstractIterator[T] {
    private[this] var remaining = n
    def advance() = remaining > 0 && { remaining -= 1; self.advance() }
    def current = self.current
  }


  /**
   * Advances this enumerator past the first ''n'' elements.
   * @param n The number of elements to be dropped
   */
  def drop(n: Int): Iterator[T] = {
    var i = 0
    while (i < n && advance()) i += 1
    this
  }

  def slice(i: Int, j: Int) = self.drop(i).take(j - i)


  /**
   * Returns a collection formed from this collection and another iterable collection by combining
   * corresponding elements in pairs. For example, [1, 2, 3] zip [-1, -2, -3] yields [(1, -1), (2, -2), (3, -3)].
   * @param that Another enumerable collection
   * @return Zipped sequence
   */
  def zip[U](that: Iterator[U]): Iterator[(T, U)] = new AbstractIterator[(T, U)] {
    def advance(): Boolean = self.advance() && that.advance()
    def current: (T, U) = (self.current, that.current)
  }

  /**
   * Returns the interleave sequence of two sequences. For example, [1, 2, 3] interleave [-1, -2, -3]
   * yields [1, -1, 2, -2, 3, -3].
   * @param that Another enumerable sequence
   * @return Interleave sequence
   */
  def interleave[U >: T](that: Iterator[U]): Iterator[U] = new AbstractIterator[U] {
    var firstSeq = true
    def advance() = {
      firstSeq = !firstSeq
      if (!firstSeq) this.advance() else that.advance()
    }
    def current = if (firstSeq) this.current else that.current
  }

  def sliding(windowSize: Int, step: Int = 1): Iterator[IndexedSeq[T]] = new AbstractIterator[IndexedSeq[T]] {
    private[this] var window = ArraySeq.withSizeHint[T](windowSize)
    private[this] var first = true
    def advance(): Boolean = {
      if (first) {
        var i = 0
        while (i < windowSize && { val t = self.advance(); if (!t) return false; t }) {
          window.appendInplace(self.current)
          i += 1
        }
        first = false
        true
      } else {
        val newWindow = ArraySeq.withSizeHint[T](windowSize)
        var i = 0
        while (i + step < windowSize) {
          newWindow.appendInplace(window(i + step))
          i += 1
        }
        while (i < windowSize && { val t = self.advance(); if (!t) return false; t }) {
          newWindow.appendInplace(self.current)
          i += 1
        }
        window = newWindow
        true
      }
    }

    def current = window
  }

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
