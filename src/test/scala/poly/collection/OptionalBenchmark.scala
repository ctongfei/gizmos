package poly.collection

import org.scalameter._
import poly.algebra.ops._
import poly.algebra.functions._
import scala.collection.mutable._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object OptionalBenchmark extends App {

  for (n ← Seq(100000, 200000, 400000, 800000, 1600000, 3200000)) {

    val conf = config(Key.exec.benchRuns → 30)
      .withWarmer(new Warmer.Default).withMeasurer(new Measurer.IgnoringGC)


    val t1 = conf measure {
      val l1 = ArrayBuffer.tabulate(n)(i => i)
    }
    println(s"Vanilla Scala ArrayBuffer: $t1")

    val l3 = mut.ArraySeq.tabulate(n)(i => i)

    val t3 = conf measure {
      val l3 = mut.ArraySeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ArraySeq: $t3")

    println()

  }

}
