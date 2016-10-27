package poly.collection

import java.util._

import org.scalatest._
import poly.algebra.syntax._
import poly.collection.testutil._
import TestUtil._
import poly.collection.mut._
/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TraversableTest extends FunSuite {

  val n = 1000

  val a = ArraySeq(1, 2, 3, 4).asTraversable
  a.chunk(2) foreach println

  def sampleArray = {
    val r = new Random()
    val n = r.nextInt(100)
    val a = Array.fill(n)(r.nextInt(100))
    a
  }

  def genVal = {
    val x = sampleArray
    val s = scala.Traversable(x: _*)
    val p = arrayAsPoly(x).asTraversable
    (s, p)
  }

  test("Equality") {
    for (i ← 0 until n) {
      val (s, p) = genVal
      s traversable_=== p
    }
  }

  test("Map") {
    for (i ← 0 until n) {
      val (s, p) = genVal
      s.map(_ * 2) traversable_=== p.map(_ * 2)
    }
  }

  test("Filter") {
    for (i ← 0 until n) {
      val (s, p) = genVal
      s.filter(_ % 2 == 1) traversable_=== p.filter(_ % 2 == 1)
    }
  }



}
