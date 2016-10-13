package poly.collection.mut

import poly.algebra.syntax._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListMapTest extends App {

  val lm = AutoMap[Int, String]()

  lm.add_!(1, "a")
  lm.add_!(2, "b")
  lm.add_!(3, "c")
  lm.add_!(4, "d")
  lm.add_!(2, "e")
  lm.remove_!(2)
  lm.remove_!(3)

  val bp = 0

}
