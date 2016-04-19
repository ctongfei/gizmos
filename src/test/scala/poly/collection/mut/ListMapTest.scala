package poly.collection.mut

import poly.algebra.syntax._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListMapTest extends App {

  val lm = AutoMap[Int, String]()

  lm.add(1, "a")
  lm.add(2, "b")
  lm.add(3, "c")
  lm.add(4, "d")
  lm.add(2, "e")
  lm.remove(2)
  lm.remove(3)

  val bp = 0

}
