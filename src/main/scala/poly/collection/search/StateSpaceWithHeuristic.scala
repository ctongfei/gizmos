package poly.collection.search


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithHeuristic[S, C] extends StateSpaceWithCost[S, C] {

  def heuristic(x: S): C

}
