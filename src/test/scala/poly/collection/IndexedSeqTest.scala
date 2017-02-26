package poly.collection

import poly.collection.mut._
import poly.algebra.syntax._

/**
  * @author Tongfei Chen
  */
object IndexedSeqTest extends App {

  val aa = ArraySeq(1, 2, 3, 4, 5).asBiSeq

  val map = aa.map(x => (x, x)) to HashMap

  val m = aa.repeat(4).groupBy(_ % 4)
  

  val a = ArraySeq(1, 2, 3, 4, 5).asIndexedSeq
  val b = ArraySeq(1, 2, 3).asIndexedSeq

  a.permuteBy(Permutation(0, 2, 1))

  val c = a product b

  val ah = a.headNode
  val ahh = a.headNode
  val bh = b.headNode

  val bp = 0
}
