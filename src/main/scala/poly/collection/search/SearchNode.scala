package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.collection.Enumerable
import poly.collection.mut.ArraySeq
import poly.collection.node.SinglePredNode

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 */

trait SearchNode[+S] extends SinglePredNode[S] {
  def state: S
  def parent: SearchNode[S]
  def depth: Int
  def data = state
  override def toString = state.toString

  def history: Enumerable[S] = Enumerable.iterate(this)(_.parent).takeUntil(_.isDummy).map(_.state).to[ArraySeq]
}




object SearchNode {
  object dummy extends SearchNode[Nothing] {
    def state: Nothing = throw new NoSuchElementException()
    def depth: Int = -1
    def parent: SearchNode[Nothing] = throw new NoSuchElementException()
    override def isDummy = true
  }

  def apply[S](s: S, p: SearchNode[S], d: Int): SearchNode[S] = new SearchNode[S] {
    def state: S = s
    def depth: Int = d
    def parent: SearchNode[S] = p
  }
}

trait SearchNodeWithF[S, N] extends SearchNode[S] {
  implicit val r: OrderedRing[N]
  def f: N
}

trait SearchNodeWithCost[S, N] extends SearchNodeWithF[S, N] {
  def g: N
  def f = g
}

trait SearchNodeWithHeuristic[S, N] extends SearchNodeWithF[S, N]  {
  def h: N
  def f = h
}

trait SearchNodeWithCostAndHeuristic[S, N] extends SearchNodeWithCost[S, N] with SearchNodeWithHeuristic[S, N] {
  override def f = g + h
}

