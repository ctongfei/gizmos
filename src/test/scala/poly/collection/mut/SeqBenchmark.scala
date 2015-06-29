package poly.collection.mut

import org.scalameter._
import poly.collection._

import scala.collection.mutable._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SeqBenchmark extends App {

  for (n ← collection.mutable.Seq(100000, 200000, 400000, 800000, 1600000)) {

    val conf = config(Key.exec.benchRuns → 50)
      .withWarmer(new Warmer.Default).withMeasurer(new Measurer.IgnoringGC)


    val t1 = conf measure {
      val l1 = ArrayBuffer.tabulate(n)(i => i)
    }
    println(s"Vanilla Scala ArrayBuffer: $t1")

    val t2 = conf measure {
      val l2 = ListBuffer.tabulate(n)(i => i)
    }
    println(s"Vanilla Scala ListBuffer: $t2")


    val t3 = conf measure {
      val l3 = mut.ArraySeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ArraySeq: $t3")
    val t4 = conf measure {
      val l3 = mut.ListSeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ListSeq: $t4")

    println()

  }

}
