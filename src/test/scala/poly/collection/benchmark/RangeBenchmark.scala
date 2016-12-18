package poly.collection.benchmark

import org.scalameter.Measurer._
import org.scalameter._
import poly.collection._
import poly.macroutil._

import scala.collection.mutable._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object RangeBenchmark extends App {

  val conf = config(Key.exec.benchRuns → 50)
    .withWarmer(new Warmer.Default).withMeasurer(new IgnoringGC)

  for (n ← scala.Seq(10000000, 20000000, 40000000, 80000000, 160000000)) {

    val tNative = conf measure {
      val l0 = Array.ofDim[Int](n)
      var i = 0
      while (i < n) {
        l0(i) = i * 2
        i += 1
      }
      //print(l0.head)
    }
    println(s"Native while loop: $tNative")

    val tPoly = conf measure {
      val l1 = Array.ofDim[Int](n)
      for (i ← 0 ~~> n)
        l1(i) = i * 2
      //print(l1.head)
    }
    println(s"Poly-collection range: $tPoly")

    val tPolyFast = conf measure {
      val l1 = Array.ofDim[Int](n)
      for (i ← (0 ~~> n).fast) {
        l1(i) = i * 2
      }
      //print(l1.head)
    }
    println(s"Poly-collection fast range: $tPolyFast")


    val tScala = conf measure {
      val l2 = Array.ofDim[Int](n)
      for (i ← 0 until n)
        l2(i) = i * 2
      //print(l2.head)
    }
    println(s"Scala until: $tScala")

    println()
  }

}
