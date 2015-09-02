package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithHeuristic[S, @sp(fdi) C] extends StateSpace[S] {

  implicit def monoidOfCost: AdditiveMonoid[C]

  def heuristic(s: S): C

  def greedyBestFirstTreeTraversal(start: S)(implicit ev1: WeakOrder[C], ev2: AdditiveMonoid[C]): Iterable[S] =
    Iterable.ofIterator(new GreedyBestFirstTreeSearchIterator[S, C](start)(ev1, ev2, this))


}
