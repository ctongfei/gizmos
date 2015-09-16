package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.collection.exception._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {
  
  implicit class withSearchingOps[S](val s: S) extends AnyVal {

    private def searchByIterator(e: SearchIterator[S], goal: S => Boolean): BiSeq[S] = {
      while (e.advance()) {
        if (goal(e.current))
          return e.currentNode.pathToRoot.map(_.data).reverse
      }
      throw new GoalNotFoundException(goal)
    }

    def depthFirstTreeSearch(goal: S => Boolean)(implicit ss: StateSpace[S]) =
      searchByIterator(new DepthFirstTreeSearchIterator[S](s)(ss), goal)

    def breadthFirstTreeSearch(goal: S => Boolean)(implicit ss: StateSpace[S]) =
      searchByIterator(new BreadthFirstTreeSearchIterator[S](s)(ss), goal)

  }
}
