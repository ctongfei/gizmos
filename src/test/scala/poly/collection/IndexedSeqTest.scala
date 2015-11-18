package poly.collection

import poly.collection.mut._

/**
  * @author Tongfei Chen
  */
object IndexedSeqTest extends App {

  val aa = ArraySeq(1, 2, 3, 4, 5).asBiSeq
  

  val a = ArraySeq(1, 2, 3, 4, 5).asIndexedSeq
  val b = ArraySeq(1, 2, 3).asIndexedSeq

  val c = a cartesianProduct b

  c foreach println

  val bp = 0
}
