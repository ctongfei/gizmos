package poly.collection.benchmark

import org.scalameter.Measurer._
import org.scalameter._
import poly.collection._
import poly.algebra.syntax._
import scala.collection.mutable._
import scala.util._

/**
 * @author Tongfei Chen
 */
object TreeMapBenchmark extends App {


  val conf = config(Key.exec.benchRuns → 30)
    .withWarmer(new Warmer.Default).withMeasurer(new IgnoringGC)

  val r = new Random()

  for (n ← scala.Seq(25000, 50000, 100000, 200000, 400000, 800000, 1600000)) {

    print(s"N = $n: ")

    val tJavaHashMap = conf measure {
      val l0 = new java.util.TreeMap[Int, String]()
      for (i ← Range(n)) {
        val x = r.nextInt()
        l0.put(x, x.toString)
      }
    }
    print(s"$tJavaHashMap, ")

    val tPolyHashMap = conf measure {
      val l2 = poly.collection.mut.RedBlackTreeMap[Int, String]()
      for (i ← Range(n)) {
        val x = r.nextInt()
        l2.addInplace(x → x.toString)
      }
    }
    print(s"$tPolyHashMap \n")
  }

}
