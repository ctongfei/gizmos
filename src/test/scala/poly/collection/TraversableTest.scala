package poly.collection

import org.scalatest._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TraversableTest extends FunSuite {

  test("Traversable functions") {

    val l = ArraySeq(3, 2, 1, 0, 4, 5, 6, 0, 1, 7)

    val f = (x: Int) => ArraySeq(x, x)

    println(l.map(_ * 2).buildString(" "))

    println(l.flatMap((x: Int) => ArraySeq.fill(x)(x)).buildString(" "))
    println(l.count(_ == 3))
    println(l.filter(_ > 4).buildString(" "))
    println(l.filterNot(_ < 4).buildString(" "))


  }

}
