package poly.collection.search

import poly.collection._
import poly.collection.exception.GoalNotFoundException
import poly.collection.mut._
import scala.util.control.Breaks._

import scala.collection.mutable

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {
  
  implicit class withSearchingOps[S](val s: S) extends AnyVal {

    def depthFirstTreeSearch(goal: S => Boolean)(implicit ss: StateSpace[S]) = {
      val dfs = new DepthFirstTreeSearchEnumerator[S](s)(ss)
      breakable {
        while (dfs.advance())
          if (goal(dfs.current)) break()
      }
      val goalNode = dfs.currentNode
      if (!goal(goalNode.state)) throw new GoalNotFoundException(goal)
      goalNode.pathToRoot.map(_.data).to[ArraySeq].reverse
    }


  }
}
