package poly.collection.search

import poly.collection.Enumerable

/**
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 */
trait StateSpace[S] {
  def succ(s: S): Enumerable[S]
}

trait StateSpaceWithCost[S, N] extends StateSpace[S] {
  def cost(from: S, to: S): N
}

trait StateSpaceWithHeuristic[S, N] extends StateSpaceWithCost[S, N] {
  def heuristic(from: S, to: S): N
}