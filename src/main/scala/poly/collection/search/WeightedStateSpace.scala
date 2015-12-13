package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait WeightedStateSpace[S, @sp(fdi) C] extends StateSpace[S] {

  implicit def groupOnCost: OrderedAdditiveGroup[C]
  def succWithCost(x: S): Traversable[(S, C)]
  def succ(x: S) = succWithCost(x) map first

  def uniformCostTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new UniformCostIterator[S, C](this, start))

  def greedyBestFirstTraversal(start: S)(heuristic: S => C): Iterable[S] =
    Iterable.ofIterator(new GreedyBestFirstIterator[S, C](this, start, heuristic))

  def aStarTraversal(start: S)(heuristic: S => C): Iterable[S] =
    Iterable.ofIterator(new AStarIterator[S, C](this, start, heuristic))

}
