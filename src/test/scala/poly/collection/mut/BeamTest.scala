package poly.collection.mut

import poly.algebra.syntax._

import org.scalatest._

/**
  * @author Tongfei Chen
  */
class BeamTest extends FunSuite {

  test("Beam") {
    val b = Beam.ofWidth[Int](3)
    b += 3
    b += 4
    b += 5
    b += 6
    b += 2
    b += 7
    b += 1
  }

}
