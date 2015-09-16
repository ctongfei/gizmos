package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.exception._
import poly.collection.mut._
import scala.language.implicitConversions

/**
 * `Iterable[T]` is the basic trait for all collections that exposes an iterator.
 * `Iterable`s differ from `Traversable`s in that the iteration process can be controlled:
 * It can be paused or resumed by the user.
 *
 * This trait is created to replace the `Iterable` Java interface or the `Iterable` Scala
 * trait in Poly-collection.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Iterable[+T] extends Traversable[T] { self =>

  import Iterable._

  /** Returns a new iterator that can be used to iterate through this collection. */
  def newIterator: Iterator[T]

  def foreach[V](f: T => V) = {
    val i = newIterator
    while (i.advance()) f(i.current)
  }

  //region HELPER FUNCTIONS

  override def map[U](f: T => U) = ofIterator {
    new AbstractIterator[U] {
      val i = self.newIterator
      def current = f(i.current)
      def advance() = i.advance()
    }
  }

  def flatMap[U](f: T => Iterable[U]) = ofIterator {
    new AbstractIterator[U] {
      val outer: Iterator[T] = self.newIterator
      var inner: Iterator[U] = Iterator.empty
      def current = inner.current
      def advance(): Boolean = {
        if (inner.advance()) true
        else {
          while (outer.advance()) {
            inner = f(outer.current).newIterator
            if (inner.advance()) return true
          }
          false
        }
      }
    }
  }

  def cartesianProduct[U](that: Iterable[U]): Iterable[(T, U)] = self.flatMap(t => that.map(u => (t, u)))

  override def filter(f: T => Boolean) = ofIterator {
    new AbstractIterator[T] {
      val i = self.newIterator
      def current: T = i.current
      def advance(): Boolean = {
        do {
          val hasNext = i.advance()
          if (!hasNext) return false
        } while (!f(i.current))
        true
      }
    }
  }

  override def filterNot(f: T => Boolean) = filterNot(x => !f(x))

  def concat[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
      private[this] var e: Iterator[U] = self.newIterator
      def advance() = {
        if (e.advance()) true else {
          e = that.newIterator
          e.advance()
        }
      }
      def current = e.current
    }
  }

  override def prepend[U >: T](u: U): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
      val i = self.newIterator
      private[this] var first = true
      private[this] var curr: U = _
      def advance() = if (first) {
        curr = u
        first = false
        true
      } else {
        val r = i.advance()
        curr = i.current
        r
      }
      def current = curr
    }
  }

  override def append[U >: T](u: U): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
      val i = self.newIterator
      private[this] var last = false
      def advance() = {
        if (last) false
        else {
          val r = i.advance()
          if (!r) last = true
          true
        }
      }
      def current = if (!last) i.current else u
    }
  }

  override def scanLeft[U >: T](z: U)(f: (U, T) => U) = ofIterator {
    new AbstractIterator[U] {
      private[this] val i = self.newIterator
      private[this] var accum = z
      private[this] var first = true
      def advance() = {
        if (first) first = false
        else accum = f(accum, i.current)
        i.advance()
      }
      def current = if (!first) accum else throw new NoSuchElementException
    }
  }

  override def scan[U >: T](z: U)(f: (U, U) => U) = scanLeft(z)(f)

  override def scanByMonoid[U >: T](m: Monoid[U]) = scanLeft(m.id)(m.op)

  override def diff[U](f: (T, T) => U) = ofIterator {
    new AbstractIterator[U] {
      private[this] val i = self.newIterator
      private[this] var a = default[T]
      private[this] var finished = i.advance()
      private[this] var b = i.current
      def advance() = {
        a = b
        finished = i.advance()
        b = i.current
        finished
      }
      def current = f(b, a)
    }
  }


  override def tail: Iterable[T] = {
    val tailIterator = self.newIterator
    tailIterator.advance()
    ofIterator(tailIterator)
  }

  override def init: Iterable[T] = ofIterator {
    new AbstractIterator[T] {
      private[this] val i = self.newIterator
      i.advance()
      private[this] var prev = default[T]
      def advance() = {
        prev = i.current
        val res = i.advance()
        res
      }
      def current = prev
    }
  }

  override def take(n: Int): Iterable[T] = ofIterator {
    new AbstractIterator[T] {
      val i = self.newIterator
      private[this] var remaining = n
      def advance() = remaining > 0 && { remaining -= 1; i.advance() }
      def current = i.current
    }
  }

  /**
   * Advances this enumerator past the first ''n'' elements.
   * @param n The number of elements to be dropped
   */
  override def skip(n: Int): Iterable[T] = {
    val skippedIterator = self.newIterator
    var i = 0
    while (i < n && skippedIterator.advance()) i += 1
    ofIterator(skippedIterator)
  }

  override def slice(i: Int, j: Int): Iterable[T] = self.skip(i).take(j - i)

  override def distinct: Iterable[T] = ???

  /**
   * Returns a collection formed from this collection and another iterable collection by combining
   * corresponding elements in pairs. For example, [1, 2, 3] zip [-1, -2, -3] yields [(1, -1), (2, -2), (3, -3)].
   * @param that Another enumerable collection
   * @return Zipped sequence
   */
  def zip[U](that: Iterable[U]): Iterable[(T, U)] = ofIterator {
    new AbstractIterator[(T, U)] {
      val it = self.newIterator
      val iu = that.newIterator
      def advance(): Boolean = it.advance() && iu.advance()
      def current: (T, U) = (it.current, iu.current)
    }
  }

  def zip3[U, V](us: Iterable[U], vs: Iterable[V]): Iterable[(T, U, V)] = ofIterator {
    new AbstractIterator[(T, U, V)] {
      val ti = self.newIterator
      val ui = us.newIterator
      val vi = vs.newIterator
      def advance(): Boolean = ti.advance() && ui.advance() && vi.advance()
      def current: (T, U, V) = (ti.current, ui.current, vi.current)
    }
  }

  /**
   * Returns the interleave sequence of two sequences. For example, [1, 2, 3] interleave [-1, -2, -3]
   * yields [1, -1, 2, -2, 3, -3].
   * @param that Another enumerable sequence
   * @return Interleave sequence
   */
  def interleave[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
      val it = self.newIterator
      val iu = that.newIterator
      var first = true
      def advance() = {
        first = !first
        if (!first) it.advance() else iu.advance()
      }
      def current = if (first) it.current else iu.current
    }
  }

  def sliding(windowSize: Int, step: Int = 1) = ofIterator {
    new AbstractIterator[IndexedSeq[T]] {
      val it = self.newIterator
      private[this] var window = ArraySeq.withSizeHint[T](windowSize)
      private[this] var first = true
      def advance(): Boolean = {
        if (first) {
          var i = 0
          while (i < windowSize && { val t = it.advance(); if (!t) return false; t }) {
            window.appendInplace(it.current)
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
          while (i < windowSize && { val t = it.advance(); if (!t) return false; t }) {
            newWindow.appendInplace(it.current)
            i += 1
          }
          window = newWindow
          true
        }
      }
      def current = window
    }
  }

  //endregion


  //region symbolic aliases
  override def +:[U >: T](u: U): Iterable[U] = prepend(u)
  override def :+[U >: T](u: U): Iterable[U] = append(u)
  def ++[U >: T](that: Iterable[U]) = this concat that
  def ×[U](that: Iterable[U]) = this cartesianProduct that
  override def |>[U](f: T => U) = this map f
  def ||>[U](f: T => Iterable[U]) = this flatMap f
  //endregion
}

object Iterable {

  object empty extends Iterable[Nothing] {
    def newIterator: Iterator[Nothing] = new AbstractIterator[Nothing] {
      def advance() = false
      def current = throw new NoSuchElementException
    }
  }

  /** Creates an iterable collection based on an existing iterator. */
  def ofIterator[T](e: => Iterator[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = e // call-by-name parameter because Iterators are mutable objects that contain states!
  }

  def single[T](x: T) = ofIterator {
    new AbstractIterator[T] {
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
  }

  /**
   * Constructs an infinite collection that is generated by repeatedly applying a given function to
   * a start value. $LAZY
   * @param s Start value
   * @param f Transition unction
   * @return An infinite sequence
   */
  def iterate[T](s: T)(f: T => T) = ofIterator {
    new AbstractIterator[T] {
      private[this] var curr: T = _
      private[this] var first = true
      def advance() = {
        if (first) {
          first = false
          curr = s
        } else curr = f(curr)
        true
      }
      def current = curr
    }
  }

  def infinite[T](x: T) = ofIterator {
    new AbstractIterator[T] {
      def advance() = true
      def current = x
    }
  }

  /** Returns the natural monad on Iterables. */
  implicit object Monad extends Monad[Iterable] {
    def flatMap[X, Y](mx: Iterable[X])(f: (X) => Iterable[Y]): Iterable[Y] = mx.flatMap(f)
    def id[X](u: X): Iterable[X] = Iterable.single(u)
  }

  implicit def fromOption[T](o: Option[T]): Iterable[T] = o match {
    case Some(x) => Iterable.single(x)
    case None    => Iterable.empty
  }
}

abstract class AbstractIterable[+T] extends AbstractTraversable[T] with Iterable[T]
