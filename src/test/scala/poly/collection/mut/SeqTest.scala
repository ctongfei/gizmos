package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SeqTest {

  def main(args: Array[String]): Unit = {

    val a = ArraySeq(1, 2, 3, 4, 5, 6)
    val b = ListSeq(1, 2, 3)
    val z = SortedArraySeq(1, 2, 3, 4, 5, 6)

    println(z.contains(8))
    println(z.contains(3))

    b.reverseInplace()

    val aa = a.sort
    val amin = aa.min

    val c = a.asIfSorted merge b.asIfSorted

    a.append_!(7)
    a.prepend_!(0)
    a.prepend_!(-1)
    a.insert_!(4, 4)
    a.delete_!(6)

    b.append_!(4)
    b.prepend_!(0)
    b.insert_!(0, 1)
    b.delete_!(3)

    val d = ArraySeq.tabulate(10)(x => util.Random.nextInt(500))

    d.sortInplace()

    d.reverseInplace()

    val e = ArraySeq.tabulate(10)(x => x)

    println(e)

    for (ew ← e.sliding(4, 1))
      println(ew)

    e foreach println

    for (x ← a) println(x)
    for (x ← b) println(x)
  }

}
