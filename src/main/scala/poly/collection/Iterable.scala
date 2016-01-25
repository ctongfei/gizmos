package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.mut._
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.language.implicitConversions

/**
 * The basic trait for all collections that exposes an iterator.
 *
 * `Iterable`s differ from `Traversable`s in that the iteration process can be controlled:
 * It can be paused or resumed by the user.
 * @author Tongfei Chen
 * @since 0.1.0
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
    new Iterator[U] {
      private[this] val i = self.newIterator
      def current = f(i.current)
      def advance() = i.advance()
    }
  }

  def flatMap[U](f: T => Iterable[U]) = ofIterator {
    new Iterator[U] {
      private[this] val outer: Iterator[T] = self.newIterator
      private[this] var inner: Iterator[U] = Iterator.empty
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
    new Iterator[T] {
      private[this] val i = self.newIterator
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

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

  def concat[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new Iterator[U] {
      private[this] var e: Iterator[U] = self.newIterator
      private[this] var first = true
      def advance() = {
        if (e.advance()) true
        else if (first) {
          e = that.newIterator
          first = false
          e.advance()
        }
        else false
      }
      def current = e.current
    }
  }

  override def prepend[U >: T](u: U): Iterable[U] = ofIterator {
    new Iterator[U] {
      private[this] val i = self.newIterator
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
    new Iterator[U] {
      private[this] val i = self.newIterator
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

  override def scanLeft[U](z: U)(f: (U, T) => U) = ofIterator {
    new Iterator[U] {
      private[this] val i = self.newIterator
      private[this] var accum = z
      private[this] var first = true
      def advance() = {
        if (first) {
          first = false
          true
        }
        else {
          val r = i.advance()
          if (r) accum = f(accum, i.current)
          r
        }
      }
      def current = accum
    }
  }

  override def scan[U >: T](z: U)(f: (U, U) => U) = scanLeft(z)(f)

  override def scanByMonoid[U >: T : Monoid] = scanLeft(id)(_ op _)

  override def consecutive[U](f: (T, T) => U) = ofIterator {
    new Iterator[U] {
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

  override def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) => U.op(y, U.inv(x)))

  override def tail: Iterable[T] = {
    val tailIterator = self.newIterator
    tailIterator.advance()
    ofIterator(tailIterator)
  }

  override def init: Iterable[T] = ofIterator {
    new Iterator[T] {
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

  override def suffixes = Iterable.iterate(self)(_.tail).takeTo(_.isEmpty)

  override def take(n: Int): Iterable[T] = ofIterator {
    new Iterator[T] {
      private[this] val i = self.newIterator
      private[this] var remaining = n
      def advance() = remaining > 0 && { remaining -= 1; i.advance() }
      def current = i.current
    }
  }

  /**
   * Advances this iterator past the first ''n'' elements.
   * @param n The number of elements to be skipped
   */
  override def skip(n: Int): Iterable[T] = {
    val skippedIterator = self.newIterator
    var i = 0
    while (i < n && skippedIterator.advance()) i += 1
    ofIterator(skippedIterator)
  }

  override def skipWhile(f: T => Boolean): Iterable[T] = {
    val skippedIterator = self.newIterator
    while (skippedIterator.advance() && f(skippedIterator.current)) {}
    ofIterator(skippedIterator)
  }

  override def takeWhile(f: T => Boolean) = ofIterator {
    new Iterator[T] {
      private[this] val i = self.newIterator
      def advance() = i.advance() && f(i.current)
      def current = i.current
    }
  }

  override def takeTo(f: T => Boolean) = ofIterator {
    new Iterator[T] {
      private[this] val i = self.newIterator
      private[this] var satisfied = false
      def advance() = {
        val r = (!satisfied) && i.advance()
        if (r) satisfied = f(current)
        r
      }
      def current = i.current
    }
  }

  override def takeUntil(f: T => Boolean) = takeWhile(x => !f(x))

  override def slice(i: Int, j: Int): Iterable[T] = self.skip(i).take(j - i)

  override def distinct[T1 >: T](implicit T1: Equiv[T1]): Iterable[T] = ???

  def union[T1 >: T](that: Iterable[T1]): Iterable[T1] = (this concat that).distinct

  def intersect[T1 >: T](that: Iterable[T1]): Iterable[T1] = ???

  override def rotate(n: Int): Iterable[T] = self.skip(n) ++ self.take(n)

  /** Pretends that this iterable collection is sorted. */
  def asIfSorted[U >: T : WeakOrder]: SortedIterable[T@uv] = new SortedIterable[T] {
    def order = implicitly[WeakOrder[U]]
    def newIterator = self.newIterator
  }

  /**
   * Returns a collection formed from this collection and another iterable collection by combining
   * corresponding elements in pairs. `|~|` is a symbolic alias of this method.
   * @param that Another enumerable collection
   * @example {{{(1, 2, 3) zip (-1, -2, -3, -4) == ((1, -1), (2, -2), (3, -3))}}}
   * @return Zipped sequence
   */
  def zip[U](that: Iterable[U]): Iterable[(T, U)] = ofIterator {
    new Iterator[(T, U)] {
      val ti = self.newIterator
      val ui = that.newIterator
      def advance(): Boolean = ti.advance() && ui.advance()
      def current: (T, U) = (ti.current, ui.current)
    }
  }

  def indexed: Iterable[(Int, T@uv)] = {
    val paired = ofIterator {
      new Iterator[(Int, T)] {
        private[this] var idx = -1
        private[this] val itr = self.newIterator
        def current = (idx, itr.current)
        def advance() = {
          idx += 1
          itr.advance()
        }
      }
    }
    paired.asIfSorted[(Int, T)](WeakOrder by first)
  }

  /**
   * Returns the interleave sequence of two sequences.
   * @param that Another enumerable sequence
   * @example {{{(1, 2, 3, 4) interleave (-1, -2, -3) == (1, -1, 2, -2, 3, -3)}}}
   * @return Interleave sequence
   */
  def interleave[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new Iterator[U] {
      private[this] val ti = self.newIterator
      private[this] val ui = that.newIterator
      private[this] var first = false
      def advance() = {
        first = !first
        if (!first) ti.advance() else ui.advance()
      }
      def current = if (!first) ti.current else ui.current
    }
  }

  /**
    * Groups elements in fixed size blocks by passing a sliding window over them.
    * @param windowSize The size of the sliding window
    * @param step Step size. The default value is 1.
    * @example {{{(1, 2, 3, 4).sliding(2) == ((1, 2), (2, 3), (3, 4))}}}
    */
  def sliding(windowSize: Int, step: Int = 1) = ofIterator {
    new Iterator[IndexedSeq[T]] {
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
          while (i + 1 < windowSize) {
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

  override def repeat(n: Int): Iterable[T] = Range(n).flatMap((i: Int) => self)

  override def cycle: Iterable[T] = ofIterator {
    new Iterator[T] {
      private[this] var outer = self.newIterator
      def current = outer.current
      def advance() = {
        val notComplete = outer.advance()
        if (!notComplete) {
          outer = self.newIterator
          outer.advance()
        }
        true
      }
    }
  }

  //endregion


  //region symbolic aliases
  override def +:[U >: T](u: U): Iterable[U] = this prepend u
  override def :+[U >: T](u: U): Iterable[U] = this append u
  def ++[U >: T](that: Iterable[U]) = this concat that
  override def |>[U](f: T => U) = this map f
  def ||>[U](f: T => Iterable[U]) = this flatMap f
  override def |?(f: T => Boolean) = this filter f
  def |×|[U](that: Iterable[U]) = this cartesianProduct that
  def |~|[U](that: Iterable[U]) = this zip that
  //endregion

  def asIterable: Iterable[T] = new AbstractIterable[T] {
    def newIterator = self.newIterator
  }

}

object Iterable {

  object empty extends Iterable[Nothing] {
    def newIterator: Iterator[Nothing] = new Iterator[Nothing] {
      def advance() = false
      def current = throw new DummyNodeException
    }
  }

  /** Creates an iterable collection based on an existing iterator. */
  def ofIterator[T](e: => Iterator[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = e // call-by-name parameter because Iterators are mutable objects that contain states!
  }

  def single[T](x: T) = ofIterator {
    new Iterator[T] {
      private[this] var curr: T = _
      private[this] var first = false
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
    new Iterator[T] {
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

  def infinite[T](x: => T) = ofIterator {
    new Iterator[T] {
      def current = x
      def advance() = true
    }
  }

  def zipN[T](xss: Iterable[T]*) = ofIterator {
    new Iterator[IndexedSeq[T]] {
      private[this] val is = ArraySeq.tabulate(xss.length)(i => xss(i).newIterator)
      def current = is.map(_.current)
      def advance() = is.forall(_.advance())
    }
  }

  /** Returns the natural monad on Iterables. */
  implicit object Monad extends ConcatenativeMonad[Iterable] {
    def flatMap[X, Y](mx: Iterable[X])(f: X => Iterable[Y]): Iterable[Y] = mx.flatMap(f)
    override def map[X, Y](mx: Iterable[X])(f: X => Y): Iterable[Y] = mx.map(f)
    def id[X](u: X): Iterable[X] = Iterable.single(u)
    def empty[X]: Iterable[X] = Iterable.empty
    def concat[X](a: Iterable[X], b: Iterable[X]) = a.concat(b)
    override def filter[X](mx: Iterable[X])(f: X => Boolean) = mx.filter(f)
  }

  object ZipApplicative extends ApplicativeFunctor[Iterable] {
    def id[X](u: X) = Iterable.infinite(u)
    def liftedMap[X, Y](mx: Iterable[X])(mf: Iterable[X => Y]) = (mx zip mf) map { case (x, f) => f(x) }
    override def product[X, Y](mx: Iterable[X])(my: Iterable[Y]) = mx zip my
  }

  /** Implicitly converts an `Option` to an `Iterable` that contains one or zero element. */
  implicit def fromOption[T](o: Option[T]): Iterable[T] = o match {
    case Some(x) => Iterable.single(x)
    case None    => Iterable.empty
  }


  implicit class ofIterables[T](val underlying: Iterable[Iterable[T]]) extends AnyVal {
    /**
      * "Flattens" this collection of collection into one collection.
      * @example {{{((1, 2, 3), (4, 5), (), (7)).flatten == (1, 2, 3, 4, 5, 7)}}}
      */
    def flatten: Iterable[T] = underlying.flatMap(identity)
  }

  implicit class ofPairs[A, B](val underlying: Iterable[(A, B)]) extends AnyVal {

    def unzip = (underlying map first, underlying map second)

  }
}

abstract class AbstractIterable[+T] extends AbstractTraversable[T] with Iterable[T]
