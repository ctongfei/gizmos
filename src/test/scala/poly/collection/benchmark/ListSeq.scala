package poly.collection.benchmark

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListSeq extends App {

  for (i ← Range(100000)) {
    val ls = mut.ListSeq.tabulate(1000000)(i => i)

  }
  val bp = 0
}
