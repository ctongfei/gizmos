package poly.collection.benchmark

import org.scalameter.Measurer._
import org.scalameter._
import poly.collection._

import scala.collection.mutable._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object SetBenchmark extends App {


  val conf = config(Key.exec.benchRuns → 30)
    .withWarmer(new Warmer.Default).withMeasurer(new IgnoringGC)

  for (n ← scala.Seq(100000, 200000, 400000, 800000, 1600000, 3200000)) {

    val tJavaHashSet = conf measure {
      val l0 = new java.util.HashSet[Int](n)
      for (i ← Range(n))
        l0.add(i)
    }
    println(s"$tJavaHashSet, ")

    val tScalaHashSet = conf measure {
      val l1 = scala.collection.mutable.HashSet[Int]()
      for (i ← Range(n))
        l1 += i
    }
    println(s"$tScalaHashSet, ")


    val tPolyHashSet = conf measure {
      val l2 = poly.collection.mut.HashSet[Int]()
      for (i ← Range(n))
        l2 add i
    }
    println(s"$tPolyHashSet \n")

    println()
  }

}
