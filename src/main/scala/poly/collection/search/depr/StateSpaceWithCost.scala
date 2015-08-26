package poly.collection.search.depr

import poly.algebra._
import poly.collection._

trait StateSpaceWithCost[S, C] extends StateSpace[S] {
  def succ(x: S): Enumerable[S] = succWithCost(x).map(_._1)
  def succWithCost(x: S): Enumerable[(S, C)]
}

trait StateSpaceWithHeuristic[S, C] extends StateSpaceWithCost[S, C] {
  /** The heuristic metric space on states. */
  def heuristic: MetricSpace[S, C]
}
