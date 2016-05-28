package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.mut._
import scala.annotation.unchecked.{uncheckedVariance ⇒ uv}
import scala.language.implicitConversions

/**
 * The basic trait for all collections that exposes an iterator.
 *
 * `Iterable`s differ from [[Traversable]]s in that the iteration process can be controlled:
 * It can be paused or resumed by the user.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Iterable[+T] extends Traversable[T] { self ⇒

  import Iterable._

  /** Returns a new iterator that can be used to iterate through this collection. */
  def newIterator: Iterator[T]

  def foreach[V](f: T ⇒ V) = {
    val i = newIterator
    while (i.advance()) f(i.current)
  }

 // final def ringOnWeight = OrderedRing[Int]

  //region HELPER FUNCTIONS

  override def map[U](f: T ⇒ U): Iterable[U] = new AbstractIterable[U] {
    def newIterator = new AbstractIterator[U] {
      private[this] val i = self.newIterator
      def current = f(i.current)
      def advance() = i.advance()
    }
    override def sizeKnown = self.sizeKnown // map preserves size
    override def size = self.size
  }

  def flatMap[U](f: T ⇒ Iterable[U]) = ofIterator {
    new AbstractIterator[U] {
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

  def monadicProduct[U](that: Iterable[U]): Iterable[(T, U)] = self.flatMap(t ⇒ that.map(u ⇒ (t, u)))

  override def filter(f: T ⇒ Boolean) = ofIterator {
    new AbstractIterator[T] {
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

  override def collect[U](pf: PartialFunction[T, U]) = ofIterator {
    new AbstractIterator[U] {
      private[this] val i = self.newIterator
      private[this] var c = default[U]
      def advance(): Boolean = {
        do {
          if (!i.advance()) return false
        } while (!(pf runWith { c = _ })(i.current))
        true
      }
      def current = c
    }
  }

  override def filterNot(f: T ⇒ Boolean) = filter(x ⇒ !f(x))

  def concat[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
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
    new AbstractIterator[U] {
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
    new AbstractIterator[U] {
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

  override def scanLeft[U](z: U)(f: (U, T) ⇒ U) = ofIterator {
    new AbstractIterator[U] {
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

  override def scan[U >: T](z: U)(f: (U, U) ⇒ U) = scanLeft(z)(f)

  override def scanByMonoid[U >: T : Monoid] = scanLeft(id)(_ op _)

  override def consecutive[U](f: (T, T) ⇒ U) = ofIterator {
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

  override def diffByGroup[U >: T](implicit U: Group[U]) = consecutive((x, y) ⇒ U.op(y, U.inv(x)))

  override def tail = {
    val tailIterator = self.newIterator
    tailIterator.advance()
    ofIterator(tailIterator)
  }

  override def init = ofIterator {
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

  override def suffixes = Iterable.iterate(self)(_.tail).takeTo(_.isEmpty)

  override def take(n: Int): Iterable[T] = ofIterator {
    new AbstractIterator[T] {
      private[this] val i = self.newIterator
      private[this] var remaining = n
      def advance() = remaining > 0 && { remaining -= 1; i.advance() }
      def current = i.current
    }
  }

  /**
   * Advances this iterator past the first ''n'' elements.
   *
   * @param n The number of elements to be skipped
   */
  override def skip(n: Int): Iterable[T] = ofIterator {
    val skippedIterator = self.newIterator
    var i = 0
    while (i < n && skippedIterator.advance()) i += 1
    skippedIterator
  }

  override def skipWhile(f: T ⇒ Boolean) = ofIterator {
    val skippedIterator = self.newIterator
    while (skippedIterator.advance() && f(skippedIterator.current)) {}
    skippedIterator
  }

  override def takeWhile(f: T ⇒ Boolean) = ofIterator {
    new AbstractIterator[T] {
      private[this] val i = self.newIterator
      def advance() = i.advance() && f(i.current)
      def current = i.current
    }
  }

  override def takeTo(f: T ⇒ Boolean) = ofIterator {
    new AbstractIterator[T] {
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

  override def takeUntil(f: T ⇒ Boolean) = takeWhile(x ⇒ !f(x))

  override def slice(i: Int, j: Int) = self.skip(i).take(j - i)

  override def distinct[U >: T : Eq]: Iterable[U] = ofIterator {
    new AbstractIterator[T] {
      private[this] val set = AutoSet[U]()
      private[this] val i = self.newIterator
      def current = i.current
      def advance(): Boolean = {
        while (i.advance()) {
          if (set notContains i.current) {
            set addInplace i.current
            return true
          }
        }
        false
      }
    }
  }

  override def distinctBy[U: Eq](f: T ⇒ U) = ofIterator {
    new AbstractIterator[T] {
      private[this] val set = AutoSet[U]()
      private[this] val i = self.newIterator
      def current = i.current
      def advance(): Boolean = {
        while (i.advance()) {
          val u = f(i.current)
          if (set notContains u) {
            set addInplace u
            return true
          }
        }
        false
      }
    }
  }

  def union[U >: T : Eq](that: Iterable[U]): Iterable[U] = (this concat that).distinct

  def intersect[U >: T : Eq](that: Iterable[U]): Iterable[U] = (this filter that.to(AutoSet)).distinct

  override def rotate(n: Int): Iterable[T] = self.skip(n) ++ self.take(n)

  /**
   * Lazily splits this collection into multiple subsequences using the given delimiter predicate.
   * @param delimiter Predicate that determines whether an element is a delimiter.
   */
  def splitBy(delimiter: T ⇒ Boolean): Iterable[Seq[T]] = ofIterator {
    new AbstractIterator[Seq[T]] {
      private[this] val i = self.newIterator
      private[this] var buf: ArraySeq[T] = null
      private[this] var complete = false
      def current = buf
      def advance(): Boolean = {
        if (complete) return false
        buf = ArraySeq[T]()
        while (i.advance()) {
          if (delimiter(i.current)) return true
          else buf :+= i.current
        }
        complete = true
        true
      }
    }
  }

  /** Lazily splits this collection into multiple subsequences using the given delimiter. */
  def split[U >: T : Eq](delimiter: U) = splitBy(x ⇒ delimiter === x)

  /**
   * Pretends that this sequence is sorted under the given implicit order.
   *
   * @param T The implicit order
   * @note Actual orderedness is not guaranteed! The user should make sure that it is sorted.
   */
  def asIfSorted(implicit T: Order[T]): SortedIterable[T @uv] = new SortedIterable[T] {
    def orderOnElements = T
    def newIterator = self.newIterator
  }

  /**
   * Returns a collection formed from this collection and another iterable collection by combining
   * corresponding elements in pairs. `|~|` is a symbolic alias of this method.
   *
   * @param that Another iterable collection
   * @example {{{(1, 2, 3) zip (-1, -2, -3, -4) == ((1, -1), (2, -2), (3, -3))}}}
   * @return Zipped sequence
   */
  def zip[U](that: Iterable[U]): Iterable[(T, U)] = ofIterator {
    new AbstractIterator[(T, U)] {
      val ti = self.newIterator
      val ui = that.newIterator
      def advance(): Boolean = ti.advance() && ui.advance()
      def current: (T, U) = (ti.current, ui.current)
    }
  }

  /**
   * Equivalent to "`this zip that map f`" but may be more efficient.
   *
   * @example {{{
   *   (1, 2, 3) zipWith (2, 3, 4) (_+_) == (3, 5, 7)
   * }}}
   */
  def zipWith[U, V](that: Iterable[U])(f: (T, U) ⇒ V): Iterable[V] = ofIterator {
    new AbstractIterator[V] {
      val ti = self.newIterator
      val ui = that.newIterator
      def current = f(ti.current, ui.current)
      def advance() = ti.advance() && ui.advance()
    }
  }

  override def withIndex: SortedIterable[(Int, T @uv)] = {
    val paired = ofIterator {
      new AbstractIterator[(Int, T)] {
        private[this] var idx = -1
        private[this] val itr = self.newIterator
        def current = (idx, itr.current)
        def advance() = {
          idx += 1
          itr.advance()
        }
      }
    }
    paired.asIfSorted(Order by first)
  }

  /**
   * Returns the interleave sequence of two sequences.
   *
   * @param that Another enumerable sequence
   * @example {{{(1, 2, 3, 4) interleave (-1, -2, -3) == (1, -1, 2, -2, 3, -3)}}}
   * @return Interleave sequence
   */
  def interleave[U >: T](that: Iterable[U]): Iterable[U] = ofIterator {
    new AbstractIterator[U] {
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

  //def chunk(chunkSize: Int) =

  /**
   * Groups elements in fixed size blocks by passing a sliding window over them.
   *
   * @param windowSize The size of the sliding window
   * @param step Step size. The default value is 1.
   * @example {{{(1, 2, 3, 4).sliding(2) == ((1, 2), (2, 3), (3, 4))}}}
   */
  def sliding(windowSize: Int, step: Int = 1) = ofIterator { //TODO: not working when step > 1
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

  override def repeat(n: Int): Iterable[T] = Range(n).flatMap((i: Int) ⇒ self)

  override def cycle: Iterable[T] = ofIterator {
    new AbstractIterator[T] {
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

  override def asIfSizeKnown(s: Int): Iterable[T] = new AbstractIterable[T] {
    def newIterator = self.newIterator
    override def foreach[V](f: T ⇒ V) = self.foreach(f)
    override def sizeKnown = true
    override def size = s
  }

  //endregion

  //region symbolic aliases
  override def +:[U >: T](u: U): Iterable[U] = this prepend u
  override def :+[U >: T](u: U): Iterable[U] = this append u
  def ++[U >: T](that: Iterable[U]) = this concat that
  //def *(n: Int) = this repeat n
  def |*|[U](that: Iterable[U]) = this monadicProduct that
  def |~|[U](that: Iterable[U]) = this zip that
  //endregion

  def asIterable: Iterable[T] = ofIterator(self.newIterator)

  // toString: inherit from Traversable
  // hashCode: by reference
  // equals: by reference

}

object Iterable {

  object empty extends Iterable[Nothing] {
    def newIterator: Iterator[Nothing] = new AbstractIterator[Nothing] {
      def advance() = false
      def current = throw new DummyNodeException
    }
  }

  /** Creates an iterable collection based on an existing iterator. */
  def ofIterator[T](e: ⇒ Iterator[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = e // call-by-name parameter because Iterators are mutable objects that contain states!
  }

  /** Creates an iterable collection that contains only one element. */
  def single[T](x: ⇒ T) = ofIterator {
    new AbstractIterator[T] {
      private[this] var curr: T = _
      private[this] var first = true
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
   * Constructs a lazy infinite collection that is generated by repeatedly applying a given function to
   * a starting value. $LAZY
   *
   * @param s Start value
   * @param f Transition function
   * @return An infinite sequence
   */
  def iterate[T](s: T)(f: T ⇒ T) = ofIterator {
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

  /** Constructs an infinite iterable stream of the given value. */
  def infinite[T](x: ⇒ T) = ofIterator {
    new AbstractIterator[T] {
      def current = x
      def advance() = true
    }
  }

  def zipMany[T](xss: Iterable[T]*) = ofIterator {
    new AbstractIterator[IndexedSeq[T]] {
      private[this] val is = ArraySeq.tabulate(xss.length)(i ⇒ xss(i).newIterator)
      def current = is map { _.current }
      def advance() = is forall { _.advance() }
    }
  }

  /** Returns the natural monad on Iterables. */
  implicit object Monad extends ConcatenativeMonad[Iterable] {
    def flatMap[X, Y](mx: Iterable[X])(f: X ⇒ Iterable[Y]): Iterable[Y] = mx.flatMap(f)
    override def map[X, Y](mx: Iterable[X])(f: X ⇒ Y): Iterable[Y] = mx.map(f)
    def id[X](u: X): Iterable[X] = Iterable.single(u)
    def empty[X]: Iterable[X] = Iterable.empty
    def concat[X](a: Iterable[X], b: Iterable[X]) = a.concat(b)
    override def filter[X](mx: Iterable[X])(f: X ⇒ Boolean) = mx.filter(f)
    override def productMap[X, Y, Z](mx: Iterable[X], my: Iterable[Y])(f: (X, Y) ⇒ Z) = (mx zipWith my)(f)
  }

  object ZipIdiom extends Idiom[Iterable] {
    def id[X](u: X) = Iterable.infinite(u)
    def liftedMap[X, Y](mx: Iterable[X])(mf: Iterable[X ⇒ Y]) = (mx zip mf) map { case (x, f) ⇒ f(x) }
    override def product[X, Y](mx: Iterable[X])(my: Iterable[Y]) = mx zip my
  }

  /** Implicitly converts an `Option` to an `Iterable` that contains one or zero element. */
  implicit def fromOption[T](o: Option[T]): Iterable[T] = o match {
    case Some(x) ⇒ Iterable.single(x)
    case None    ⇒ Iterable.empty
  }


  implicit class IterableOfIterablesOps[T](val underlying: Iterable[Iterable[T]]) extends AnyVal {
    /**
      * "Flattens" this collection of collection into one collection.
     *
     * @example {{{((1, 2, 3), (4, 5), (), (7)).flatten == (1, 2, 3, 4, 5, 7)}}}
      */
    def flatten: Iterable[T] = underlying.flatMap(identity)
  }

  implicit class IterableOfPairsOps[A, B](val underlying: Iterable[(A, B)]) extends AnyVal {

    def unzip = (underlying map first, underlying map second)

  }
}

abstract class AbstractIterable[+T] extends AbstractTraversable[T] with Iterable[T]
