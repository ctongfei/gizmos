package poly.collection.mut

import poly.algebra.ops._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SortedArraySeqTest extends App {

  val a = SortedArraySeq(1, 2, 4, 1)
  a.add(3)
  a.add(0)
  a.add(-5)
  a.add(15)
  a.remove(4)
  a.remove(6)
  a.remove(3)
  a.deleteAt(2)
  print("")

}
