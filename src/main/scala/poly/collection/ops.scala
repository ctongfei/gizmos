package poly.collection

import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
object ops {

  implicit final class withRangeOps(val left: Int) extends AnyVal {
    /**
      * Creates a left-inclusive-right-inclusive ascending range.
      * @example {{{ (1 ~<~ 4) == (1, 2, 3, 4) }}}
      */
    @inline def ~<~(right: Int) = Range.inclusive(left, right)

    /**
      * Creates a left-inclusive-right-inclusive descending range.
      * @example {{{ (4 ~>~ 1) == (4, 3, 2, 1) }}}
      */
    @inline def ~>~(right: Int) = Range.inclusive(left, right, -1)

    /**
      * Creates a left-inclusive-right-exclusive ascending range.
      * @example {{{ (0 ~~< 4) == (0, 1, 2, 3) }}}
      */
    @inline def ~~<(right: Int) = Range(left, right)

    /**
      * Creates a left-inclusive-right-exclusive descending range.
      * @example {{{ (4 ~~> 0) == (4, 3, 2, 1) }}}
      */
    @inline def ~~>(right: Int) = Range(left, right, -1)

    /**
      * Creates a left-exclusive-right-inclusive descending range.
      * @example {{{ (4 >~~ 0) == (3, 2, 1, 0) }}}
      */
    @inline def >~~(right: Int) = Range(right, left).reverse

    /**
      * Creates a left-exclusive-right-inclusive ascending range.
      * @example {{{ (0 <~~ 4) == (1, 2, 3, 4) }}}
      */
    @inline def <~~(right: Int) = Range(right, left, -1).reverse
  }


  implicit class withCollectionOps[T](val x: T) extends AnyVal {

    /**
     * Constructs a sequence of length 1 with this specific element.
     * @return A sequence with only `this` element
     * @example {{{3.single == (3)}}}
     */
    def single = IndexedSeq.fill(1)(x)

    /**
     * Constructs a lazy sequence that repeats the specific element infinitely.
     * @return An infinite sequence
     * @example {{{2.cycle == (2, 2, 2, 2, 2, ...)}}}
     */
    def cycle = Iterable.infinite(x)

    /**
     * Constructs a lazy sequence that repeats the specific element for the given number of times.
     * @example {{{4.repeat(5) == (4, 4, 4, 4, 4)}}}
     */
    def repeat(n: Int) = IndexedSeq.fill(n)(x)

    /**
     * Constructs a lazy infinite sequence by iteratively applying a function from a starting element.
     * @example {{{0.iterate(_ + 2) == (0, 2, 4, 6, 8, ...)}}}
     * @return
     */
    def iterate(f: T => T) = Seq.iterate(x)(f)

    def unfold[U](f: T => Option[(U, T)]): Seq[U] = ??? //TODO

  }

}
