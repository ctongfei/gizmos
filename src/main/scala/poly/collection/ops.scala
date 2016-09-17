package poly.collection

import poly.algebra._
import poly.collection.node._
import poly.collection.search._

import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait ImplicitOperators {

  implicit final class OptionWithMonadOps[T](val a: Option[T]) {

    def zip[U](b: Option[U]): Option[(T, U)] = for (x <- a; y <- b) yield (x, y)

    def zipWith[U, V](b: Option[U])(f: (T, U) => V): Option[V] = for (x <- a; y <- b) yield f(x, y)

  }

  implicit final class withRangeOps(val left: Int) {
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
    @inline def <~~(right: Int) = (right ~~> left).reverse
  }


  implicit class withCollectionOps[T](x: => T) { // call-by-name

    /** Checks if this element belongs to the specific set. */
    def in[U >: T](set: Set[U]) = set contains x

    /** Checks if this element does not belong to the specific set. */
    def notIn[U >: T](set: Set[U]) = set notContains x

    /** Checks if this element belongs to the specific set. */
    def ∈[U >: T](set: Set[U]) = set contains x

    /** Checks if this element does not belong to the specific set. */
    def ∉[U >: T](set: Set[U]) = set notContains x

    /** Checks if this element belongs to the specific multiset. */
    def in[U >: T](mSet: Multiset[U, _]) = mSet contains x

    /** Checks if this element does not belong to the specific multiset. */
    def notIn[U >: T](mSet: Multiset[U, _]) = mSet notContains x

    /**
     * Constructs a sequence of length 1 with this specific element.
     * @return A sequence with only `this` element
     * @example {{{3.single == (3)}}}
     */
    def single = IndexedSeq.fill(1)(x)

    /**
     * Constructs a lazy sequence that repeats the specific element infinitely.
     * @return An infinite sequence
     * @example {{{2.infinitely == (2, 2, 2, 2, 2, ...)}}}
     */
    def infinitely = Seq.infinite(x)

    /**
     * Constructs a lazy infinite sequence by iteratively applying a function from a starting element.
     * @example {{{0.iterate(_ + 2) == (0, 2, 4, 6, 8, ...)}}}
     * @return
     */
    def iterate(f: T => T) = Seq.iterate(x)(f)

    def unfold[A](f: T => (A, Option[T])): Seq[A] = {
      class UnfoldedNode(val state: Option[T]) extends SeqNode[A] {
        private[this] val m = state map f
        def data = m.get._1
        def next = new UnfoldedNode(m.get._2)
        def isDummy = m.isEmpty
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

    def unfoldToBinaryTree[A](f: T => (A, Option[T], Option[T])): BinaryTree[A] = {
      class UnfoldedNode2(val state: Option[T]) extends BinaryTreeNode[A] {
        private[this] val m = state map f
        def leftNode = new UnfoldedNode2(m.get._2)
        def rightNode = new UnfoldedNode2(m.get._3)
        def isDummy = m.isEmpty
        def data = m.get._1
      }
      BinaryTree.ofRootNode(new UnfoldedNode2(Some(x)))
    }

    def unfoldToBinaryTreeInfinitely[A](f: T => (A, T, T)): BinaryTree[A] = {
      class InfinitelyUnfoldedNode2(val state: T) extends BinaryTreeNode[A] {
        val (data, leftState, rightState) = f(state)
        def leftNode = new InfinitelyUnfoldedNode2(leftState)
        def rightNode = new InfinitelyUnfoldedNode2(rightState)
        def isDummy = false
      }
      BinaryTree.ofRootNode(new InfinitelyUnfoldedNode2(x))
    }

  }

}
