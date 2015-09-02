package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._
import poly.util.specgroup._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SearchNodeWithHeuristic[+S, @sp(fdi) C] extends SearchNode[S] {

  def parent: SearchNodeWithHeuristic[S, C]

  /** The heuristic estimate of the cost from this node to the goal. */
  def h: C
}


object SearchNodeWithHeuristic {

  implicit def order[S, @sp(fdi) C: WeakOrder : AdditiveMonoid]: WeakOrder[SearchNodeWithHeuristic[S, C]] =
    new WeakOrder[SearchNodeWithHeuristic[S, C]] {
      def cmp(x: SearchNodeWithHeuristic[S, C], y: SearchNodeWithHeuristic[S, C]) = x.h >?< y.h
    }

  def apply[S, @sp(fdi) C](s: S, d: Int, hv: C, p: SearchNodeWithHeuristic[S, C]): SearchNodeWithHeuristic[S, C] = new SearchNodeWithHeuristic[S, C] {
    val state = s
    val depth = d
    val h = hv
    val parent = p
  }

  def dummy[@sp(fdi) C]: SearchNodeWithHeuristic[Nothing, C] = new SearchNodeWithHeuristic[Nothing, C] {
    def state: Nothing = throw new NoSuchElementException()
    def depth: Int = -1
    def h = throw new NoSuchElementException()
    def parent: SearchNodeWithHeuristic[Nothing, C] = this
    override def isDummy = true
  }
}

