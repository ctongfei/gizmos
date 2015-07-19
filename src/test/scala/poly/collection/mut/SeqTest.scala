package poly.collection.mut

import poly.algebra.implicits._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SeqTest {

  def main(args: Array[String]): Unit = {

    val a = ArraySeq[Int](1, 2, 3, 4, 5, 6)
    val b = ListSeq[Int](1, 2, 3)

    val c = a.asIfSorted merge b.asIfSorted

    a.append(7)
    a.prepend(0)
    a.prepend(-1)
    a.insertAt(4, 4)
    a.deleteAt(6)

    b.append(4)
    b.prepend(0)
    b.insertAt(0, 1)
    b.deleteAt(3)

    val d = ArraySeq.tabulate(100)(x => util.Random.nextInt(500))

    d.inplaceSort()

    d.inplaceReverse()

    for (x ← a) println(x)
    for (x ← b) println(x)
  }

}
