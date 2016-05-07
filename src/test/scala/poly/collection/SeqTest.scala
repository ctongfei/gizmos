package poly.collection

import poly.collection.mut._
import poly.collection.ops._
/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object SeqTest extends App {

  for (i ← 0 ~~< 4) println(i)


  val a = ListSeq[Int](3, 3, 2)
  a appendInplace 0
  a appendInplace 2
  a appendInplace 3
  a appendInplace 4


  val b = ArraySeq(5, 6, 7)

  val t0 = a map { i => i * 2 }
  val t1 = a flatMap { i: Int => ArraySeq.fill(i)(i) }
  val t2 = a product b

  val t3 = a filter { _ % 2 == 0 }
  val t4 = a filterNot { _ > 2 }

  val t5 = a concat b
  val t6 = -1 +: a
  val t7 = b :+ 10

  val t8 = a.scanLeft(0)(_+_)
  val t9 = t8.consecutive(_+_)
  val t10 = a.suffixes

  val bp = 0


}
