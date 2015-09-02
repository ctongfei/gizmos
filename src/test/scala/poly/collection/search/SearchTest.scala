package poly.collection.search

import poly.collection.conversion._
import poly.collection.search.ops._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SearchTest extends App {


  val g = Map(0 → List(1, 2, 3), 1 → List(2, 3, 4), 2 → List(3), 3 → List(), 4 → List())

  val ss = new StateSpace[Int] {
    def succ(x: Int) = g(x)
  }

  0.depthFirstTreeSearch(_ == 4)(ss) foreach println


}
