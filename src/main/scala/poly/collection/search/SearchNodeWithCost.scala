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
trait SearchNodeWithCost[+S, @sp(fdi) C] extends SearchNode[S] {

  def parent: SearchNodeWithCost[S, C]

  /** The known cost from the initial node to this node. */
  def g: C
}


object SearchNodeWithCost {

  implicit def order[S, @sp(fdi) C: WeakOrder]: WeakOrder[SearchNodeWithCost[S, C]] = new WeakOrder[SearchNodeWithCost[S, C]] {
    def cmp(x: SearchNodeWithCost[S, C], y: SearchNodeWithCost[S, C]) = x.g >?< y.g
  }

  def apply[S, @sp(fdi) C](s: S, d: Int, gv: C, p: SearchNodeWithCost[S, C]): SearchNodeWithCost[S, C] = new SearchNodeWithCost[S, C] {
    val state = s
    val depth = d
    val g = gv
    val parent = p
    def isDummy = false
  }

  def dummy[@sp(fdi) C: AdditiveMonoid]: SearchNodeWithCost[Nothing, C] = new SearchNodeWithCost[Nothing, C] {
    def state: Nothing = throw new NoSuchElementException()
    def depth: Int = -1
    def g = zero[C]
    def parent: SearchNodeWithCost[Nothing, C] = this
    def isDummy = true
  }
}

