package poly.collection.search

import poly.collection._
import poly.collection.exception.GoalNotFoundException
import poly.collection.mut._
import poly.collection.node.SinglePredNode

import scala.collection.mutable

/**
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 */
object Implicits {
  implicit class SearchOps[S](val s: S) extends AnyVal {
    def treePathTo(isGoal: S ⇒ Boolean, fringe: Fringe[S] = new DepthFirstFringe[S](s))(implicit ss: StateSpace[S]): Enumerable[S] = {
      fringe += s
      while (fringe notEmpty) {
        val top = fringe.pop()
        if (isGoal(top)) return fringe.topNode.history
        else fringe ++= ss.succ(top)
      }
      throw new GoalNotFoundException(isGoal)
    }

    def graphPathTo(isGoal: S ⇒ Boolean, fringe: Fringe[S] = new DepthFirstFringe[S](s))(implicit ss: StateSpace[S]): Enumerable[S] = {
      val visited = mutable.HashSet[S]() //TODO: poly-ize
      fringe += s
      while (fringe notEmpty) {
        val top = fringe.pop()
        if (isGoal(top)) return fringe.topNode.history
        else fringe ++= ss.succ(top)
      }
      throw new GoalNotFoundException(isGoal)
    }
  }
}
