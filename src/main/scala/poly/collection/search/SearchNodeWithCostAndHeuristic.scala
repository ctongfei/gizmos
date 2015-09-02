package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.function._
import poly.util.specgroup._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SearchNodeWithCostAndHeuristic[+S, @sp(fdi) C] extends SearchNodeWithCost[S, C] with SearchNodeWithHeuristic[S, C] {

  def parent: SearchNodeWithCostAndHeuristic[S, C]

  def f(implicit ev: AdditiveMonoid[C]) = g + h

}


object SearchNodeWithCostAndHeuristic {

  implicit def order[S, @sp(fdi) C: WeakOrder : AdditiveMonoid]: WeakOrder[SearchNodeWithCostAndHeuristic[S, C]] =
    new WeakOrder[SearchNodeWithCostAndHeuristic[S, C]] {
      def cmp(x: SearchNodeWithCostAndHeuristic[S, C], y: SearchNodeWithCostAndHeuristic[S, C]) = x.f >?< y.f
    }

  def apply[S, @sp(fdi) C](s: S, d: Int, gv: C, hv: C, p: SearchNodeWithCostAndHeuristic[S, C]): SearchNodeWithCostAndHeuristic[S, C] =
    new SearchNodeWithCostAndHeuristic[S, C] {
      val state = s
      val depth = d
      val h = hv
      val g = gv
      val parent = p
    }

  def dummy[@sp(fdi) C: AdditiveMonoid]: SearchNodeWithCostAndHeuristic[Nothing, C] = new SearchNodeWithCostAndHeuristic[Nothing, C] {
    def state: Nothing = throw new NoSuchElementException()
    def depth: Int = -1
    def g = zero[C]
    def h = zero[C]
    def parent: SearchNodeWithCostAndHeuristic[Nothing, C] = this
    override def isDummy = true
  }
}

