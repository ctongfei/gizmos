package poly.collection.mut

import poly.algebra.syntax._
import poly.collection.testutil.TestUtil._

import scala.util._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object HashSetTest extends App {

  val pm = HashMap[Int, String](1 → "a", 2 → "b", 3 → "c")

  val pls = LinkedHashSet[Int](1, 2, 3, 4, 5, 6, 7)

  val plm = LinkedHashMap[Int, String](1 → "a", 3 → "4", 4 → "b")

  val p = HashSet[Double]()
  val s = scala.collection.mutable.HashSet[Double]()

  val r = new Random()

  for (i ← 0 until 10000) {
    val x = r.nextGaussian()
    s += x
    p addInplace x
    if ({s set_=== p; true}) println(s"CHECKED $i")
    else throw new RuntimeException()
  }

  p.elements foreach println

  val ss = s.clone()

  var i = 10000
  for (x ← ss) {
    s -= x
    p removeInplace x
    i -= 1
    if ({s set_=== p; true}) println(s"CHECKED $i")
    else throw new RuntimeException
  }

}
