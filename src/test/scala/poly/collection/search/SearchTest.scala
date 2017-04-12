package poly.collection.search

import poly.algebra.syntax._
import poly.collection._
import poly.collection.search.ops._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SearchTest extends App {


  val gm = Map(0 → Seq(1, 2, 3), 1 → Seq(2, 3, 4), 2 → Seq(3), 3 → Seq(), 4 → Seq())
  val g = gm.keySet.createGraph((i, j) => if (gm(i) contains j) Some(1) else None)
  val g0 = g

  g.depthFirstTraversal(0)

}
