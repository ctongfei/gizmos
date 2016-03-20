package poly.collection

import poly.collection.node._

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
    @inline def ~<~(right: Int) = new Range.Ascending(left, right + 1)

    /**
      * Creates a left-inclusive-right-inclusive descending range.
      * @example {{{ (4 ~>~ 1) == (4, 3, 2, 1) }}}
      */
    @inline def ~>~(right: Int) = new Range.Descending(left, right - 1, -1)

    /**
      * Creates a left-inclusive-right-exclusive ascending range.
      * @example {{{ (0 ~~< 4) == (0, 1, 2, 3) }}}
      */
    @inline def ~~<(right: Int) = new Range.Ascending(left, right)

    /**
      * Creates a left-inclusive-right-exclusive descending range.
      * @example {{{ (4 ~~> 0) == (4, 3, 2, 1) }}}
      */
    @inline def ~~>(right: Int) = new Range.Descending(left, right, -1)

    /**
      * Creates a left-exclusive-right-inclusive descending range.
      * @example {{{ (4 >~~ 0) == (3, 2, 1, 0) }}}
      */
    @inline def >~~(right: Int) = (right ~~< left).reverse

    /**
      * Creates a left-exclusive-right-inclusive ascending range.
      * @example {{{ (0 <~~ 4) == (1, 2, 3, 4) }}}
      */
    @inline def <~~(right: Int) = (left ~~> right).reverse
  }


  implicit class withCollectionOps[T](val x: T) {

    /** Checks if this element belongs to the specific set. */
    def in(set: Set[T]) = set contains x

    /** Checks if this element belongs to the specific multiset. */
    def in(mSet: Multiset[T, _]) = mSet contains x

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


    def unfold[A](f: T => (A, Option[T])): Seq[A] = {
      class UnfoldedNode(val state: Option[T]) extends SeqNode[A] {
        println(state)
        def data = f(state.get)._1
        def next = new UnfoldedNode(f(state.get)._2)
        def isDummy = state.isEmpty
      }
      Seq.ofHeadNode(new UnfoldedNode(Some(x)))
    }

    def unfoldInfinitely[A](f: T => (A, T)): Seq[A] = {
      class InfinitelyUnfoldedNode(val state: T) extends SeqNode[A] {
        val (data, nextState) = f(state)
        def next = new InfinitelyUnfoldedNode(nextState)
        def isDummy = false
      }
      Seq.ofHeadNode(new InfinitelyUnfoldedNode(x))
    }

  }

}
