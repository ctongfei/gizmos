package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithHeuristic[S, @sp(fdi) C] extends StateSpaceWithCost[S, C] {

  def heuristic(s: S): C

  def greedyBestFirstTreeTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new GreedyBestFirstTreeSearchIterator[S, C](start)(this))

  def aStarTreeTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new AStarTreeSearchIterator[S, C](start)(this))


}
