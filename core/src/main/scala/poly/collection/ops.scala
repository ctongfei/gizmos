package poly.collection

import poly.algebra._
import poly.collection.node._
import poly.collection.search._

import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait ImplicitOps {

  implicit final class OptionWithMonadOps[T](val a: Option[T]) {

    def zip[U](b: Option[U]): Option[(T, U)] = for (x <- a; y <- b) yield (x, y)

    def zipWith[U, V](b: Option[U])(f: (T, U) => V): Option[V] = for (x <- a; y <- b) yield f(x, y)

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
    def in[U >: T](mSet: WeightedSet[U, _]) = mSet contains x

    /** Checks if this element does not belong to the specific multiset. */
    def notIn[U >: T](mSet: WeightedSet[U, _]) = mSet notContains x

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

    def unfold[A](f: T => (T, A)): Seq[A] = {
      class InfinitelyUnfoldedNode(val state: T) extends SeqNode[A] {
        val (nextState, data) = f(state)
        def next = new InfinitelyUnfoldedNode(nextState)
        def isDummy = false
      }
      Seq.ofHeadNode(new InfinitelyUnfoldedNode(x))
    }

    def unfoldUntil[A](f: T => (T, A))(p: T => Boolean): Seq[A] = {
      class UnfoldedUntilNode(val state: T) extends SeqNode[A] {
        val (nextState, data) = f(state)
        def next = new UnfoldedUntilNode(nextState)
        def isDummy = p(state)
      }
      Seq.ofHeadNode(new UnfoldedUntilNode(x))
    }

    def unfoldToBinaryTree[A](f: T => (T, A, T)): BinaryTree[A] = {
      class InfinitelyUnfoldedNode2(val state: T) extends BinaryTreeNode[A] {
        val (leftState, data, rightState) = f(state)
        def leftNode = new InfinitelyUnfoldedNode2(leftState)
        def rightNode = new InfinitelyUnfoldedNode2(rightState)
        def isDummy = false
      }
      BinaryTree.ofRootNode(new InfinitelyUnfoldedNode2(x))
    }

  }

}
