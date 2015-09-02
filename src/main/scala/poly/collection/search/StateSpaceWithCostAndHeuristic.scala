package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithCostAndHeuristic[S, @sp(fdi) C] extends StateSpaceWithCost[S, C] with StateSpaceWithHeuristic[S, C] {

  def aStarTreeTraversal(start: S)(implicit ev: WeakOrder[C]): Iterable[S] =
    Iterable.ofIterator(new AStarTreeSearchIterator[S, C](start)(ev, this.monoidOfCost, this))


}
