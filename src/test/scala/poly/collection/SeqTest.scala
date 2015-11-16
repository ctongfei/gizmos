package poly.collection

import poly.collection.mut._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object SeqTest extends App {

  val a = ListSeq(0, 2, 3, 4).asSeq
  val b = ArraySeq(5, 6, 7).asSeq

  val t0 = a map { i => i * 2 }
  val t1 = a flatMap { (i: Int) => ArraySeq.fill(i)(i) }
  val t2 = a cartesianProduct b

  val t3 = a filter { _ % 2 == 0 }
  val t4 = a filterNot { _ > 2 }

  val t5 = a concat b
  val t6 = -1 +: a
  val t7 = b :+ 10

  val t8 = a.scanLeft(0)(_+_)
  val t9 = t8.consecutive(_+_)
  val t10 = a.tails

  val bp = 0


}
