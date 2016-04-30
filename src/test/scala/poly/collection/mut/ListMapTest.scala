package poly.collection.mut

import poly.algebra.syntax._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListMapTest extends App {

  val lm = AutoMap[Int, String]()

  lm.addInplace(1, "a")
  lm.addInplace(2, "b")
  lm.addInplace(3, "c")
  lm.addInplace(4, "d")
  lm.addInplace(2, "e")
  lm.removeInplace(2)
  lm.removeInplace(3)

  val bp = 0

}
