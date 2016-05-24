package poly.collection

import mut._
/**
 * @author Tongfei Chen
 */
object TableTest extends App {

  val t = Table.tabulate(3, 3)((i, j) => i + j * 0.003)

  println(t.toString)

}
