package poly.collection

import org.scalatest._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TraversableTest extends FunSuite {

  test("Traversable functions") {

    val l = ArraySeq(3, 2, 1, 0, 4, 5, 6, 7, 1, 8)

    val f = (x: Int) => ArraySeq(x, x)


    println(l.map(_ * 2).buildString(" "))

    val fml = l.flatMap((x: Int) => ArraySeq(x, x))

    println(l.flatMap((x: Int) => ArraySeq(x, x)).buildString(" "))
    println(l.count(_ == 3))
    println(l.filter(_ > 4).buildString(" "))
    println(l.filterNot(_ < 4).buildString(" "))


  }

}
