package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SearchNodeWithHeuristic[+S, C] extends SearchNodeWithCost[S, C] {

  def parent: SearchNodeWithHeuristic[S, C]

  /** The overall cost function. */
  def f = g + h

  /** The heuristic estimate of the cost from this node to the goal. */
  def h: C
}


object SearchNodeWithHeuristic {

  implicit def order[S, C: WeakOrder]: WeakOrder[SearchNodeWithHeuristic[S, C]] =
    new WeakOrder[SearchNodeWithHeuristic[S, C]] {
      def cmp(x: SearchNodeWithHeuristic[S, C], y: SearchNodeWithHeuristic[S, C]) = x.f >?< y.f
    }

  def apply[S, C](s: S, d: Int, gv: C, hv: C, p: SearchNodeWithHeuristic[S, C]): SearchNodeWithHeuristic[S, C] = new SearchNodeWithHeuristic[S, C] {
    val state = s
    val depth = d
    val h = hv
    val g = gv
    val parent = p
    implicit def numericOfCost = OrderedRing[C]
  }

  def dummy[C]: SearchNodeWithHeuristic[Nothing, C] = new SearchNodeWithHeuristic[Nothing, C] {
    def state: Nothing = throw new NoSuchElementException()
    def depth: Int = -1
    def g = zero[C]
    def h = zero[C]
    def parent: SearchNodeWithHeuristic[Nothing, C] = this
    override def isDummy = true
    implicit def numericOfCost = OrderedRing[C]
  }
}

