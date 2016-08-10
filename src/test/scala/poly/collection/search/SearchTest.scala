package poly.collection.search

import poly.algebra.syntax._
import poly.collection._
import poly.collection.search.ops._
import poly.collection.conversion.FromScala._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SearchTest extends App {


  val g = Map(0 → Seq(1, 2, 3), 1 → Seq(2, 3, 4), 2 → Seq(3), 3 → Seq(), 4 → Seq())
  val g0 = g(0)

  0.depthFirstTraversal(g)

}
