package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SeqTest {

  def main(args: Array[String]): Unit = {

    val a = ArraySeq[Int](1, 2, 3, 4, 5, 6)
    val b = ListSeq[Int](1, 2, 3)

    val aa = a.sort
    val amin = aa.min

    val c = a.asIfSorted merge b.asIfSorted

    a.appendInplace(7)
    a.prependInplace(0)
    a.prependInplace(-1)
    a.insertAt(4, 4)
    a.deleteAt(6)

    b.appendInplace(4)
    b.prependInplace(0)
    b.insertAt(0, 1)
    b.deleteAt(3)

    val d = ArraySeq.tabulate(10)(x => util.Random.nextInt(500))

    d.sortInplace()

    d.reverseInplace()

    val e = ArraySeq.tabulate(10)(x => x)

    println(e)

    for (ew ← e.sliding(4, 1))
      println(ew)

    e.pairs foreach println

    for (x ← a) println(x)
    for (x ← b) println(x)
  }

}
