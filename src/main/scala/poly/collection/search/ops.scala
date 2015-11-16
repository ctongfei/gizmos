package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.collection.exception._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {

  private def searchByIterator[S, S1 >: S](e: SearchIterator[S1], goal: S1 => Boolean): BiSeq[S1] = {
    while (e.advance()) {
      if (goal(e.current))
        return e.currentNode.pathToRoot.map(_.data).reverse
    }
    BiSeq.empty
  }

  implicit class withTreeSearchingOps[S, S1 >: S](val s: S)(implicit ss: TreeStateSpace[S1]) {

    def depthFirstTreeSearch(goal: S1 => Boolean) =
      searchByIterator(new DepthFirstTreeSearchIterator[S1](s)(ss), goal)

    def depthFirstTreeTraversal =
      Iterable.ofIterator(new DepthFirstTreeSearchIterator[S1](s)(ss))

    def breadthFirstTreeSearch(goal: S1 => Boolean) =
      searchByIterator(new BreadthFirstTreeSearchIterator[S1](s)(ss), goal)



  }
}
