package poly.collection.mut

import org.scalatest._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class ListTest extends FunSuite {

  test("ListSeq") {

  }

  test("ListBiSeq") {
    val lbs = ListBiSeq[Int](1, 2, 3, 4)

    lbs.reverseInplace()

    assert(lbs == Seq(4, 3, 2, 1))

    lbs.mapInplace(5-_)
    assert(lbs == Seq(1, 2, 3, 4))

  }

}
