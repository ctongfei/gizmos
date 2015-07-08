package poly.collection.search

import poly.collection._
import poly.collection.exception._
import poly.collection.mut._



trait StateSpaceWithCost[S, C] extends StateSpace[S] {
  def succ(x: S): Enumerable[S] = succWithCost(x).map(_._1)
  def succWithCost(x: S): Enumerable[(S, C)]
  

}

trait StateSpaceWithHeuristic[S, C] extends StateSpaceWithCost[S, C] {
  /** A heuristic estimation of the cost between two states. */
  def heuristic(x: S, y: S): C
}
