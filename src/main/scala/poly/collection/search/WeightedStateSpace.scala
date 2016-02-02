package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * Represents a search state space whose edges are weighted.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait WeightedStateSpace[S, @sp(fdi) C] extends StateSpace[S] {

  implicit def groupOnCost: OrderedAdditiveGroup[C]
  def succWithCost(x: S): Traversable[(S, C)]
  def succ(x: S) = succWithCost(x) map firstOfPair

  def uniformCostTraversal(start: S) =
    Iterable.ofIterator(new UniformCostIterator[S, C](this, start))

  def greedyBestFirstTraversal(start: S)(heuristic: S => C) =
    Iterable.ofIterator(new GreedyBestFirstIterator[S, C](this, start, heuristic))

  def aStarTraversal(start: S)(heuristic: S => C) =
    Iterable.ofIterator(new AStarIterator[S, C](this, start, heuristic))

  def uniformCostSearch(start: S, goal: S => Boolean) =
    StateSpace.searchByIterator(new UniformCostIterator[S, C](this, start), goal)

  def greedyBestFirstSearch(start: S, goal: S => Boolean)(heuristic: S => C) =
    StateSpace.searchByIterator(new GreedyBestFirstIterator[S, C](this, start, heuristic), goal)

  def aStarSearch(start: S, goal: S => Boolean)(heuristic: S => C) =
    StateSpace.searchByIterator(new AStarIterator[S, C](this, start, heuristic), goal)

}
