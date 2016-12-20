package poly.collection.benchmark

import org.scalameter.Measurer._
import org.scalameter._
import poly.collection._
import scala.util._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object BitSetBenchmark extends App {


  val conf = config(Key.exec.benchRuns → 30)
    .withWarmer(new Warmer.Default).withMeasurer(new IgnoringGC)

  val r = new Random()

  for (n ← scala.Seq(100000, 200000, 400000, 800000, 1600000, 3200000)) {

    print(s"N = $n: ")

    val tJavaBitSet = conf measure {
      val l0 = new java.util.BitSet()
      for (i ← Range(n))
        l0.set(r.nextInt(n))
    }
    print(s"$tJavaBitSet, ")

    val tScalaBitSet = conf measure {
      val l1 = scala.collection.mutable.BitSet()
      for (i ← Range(n))
        l1 += r.nextInt(n)
    }
    print(s"$tScalaBitSet, ")


    val tPolyBitSet = conf measure {
      val l2 = poly.collection.mut.BitSet()
      for (i ← Range(n))
        l2 add_! r.nextInt(n)
    }
    print(s"$tPolyBitSet \n")
  }

}
