package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.collection.exception._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {

  private def searchByIterator[S](e: SearchIterator[S], goal: S => Boolean): BiSeq[S] = {
    while (e.advance()) {
      if (goal(e.current))
        return e.currentNode.pathToRoot.map(_.data).reverse
    }
    BiSeq.empty
  }

  implicit class withSearchingOps[S, S1 >: S](val s: S)(implicit ss: StateSpace[S1]) {

    def depthFirstTreeSearch(goal: S1 => Boolean) =
      searchByIterator(new DepthFirstTreeSearchIterator[S1](s)(ss), goal)

    def depthFirstTreeTraversal =
      Iterable.ofIterator(new DepthFirstTreeSearchIterator[S1](s)(ss))

    def breadthFirstTreeSearch(goal: S1 => Boolean) =
      searchByIterator(new BreadthFirstTreeSearchIterator[S1](s)(ss), goal)



  }
}
