package poly.collection.benchmark

import org.scalameter.Measurer._
import org.scalameter._
import poly.collection._

import scala.collection.mutable._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object SeqBenchmark extends App {


  val conf = config(Key.exec.benchRuns → 50)
    .withWarmer(new Warmer.Default).withMeasurer(new IgnoringGC)

  for (n ← scala.Seq(1000000, 2000000, 3000000, 4000000, 5000000)) {

    val tNative = conf measure {
      val l0 = Array.ofDim[Int](n)
      var i = 0
      while (i < n) {
        l0(i) = i
        i += 1
      }
    }
    println(s"Native array: $tNative")

    val tArrayList = conf measure {
      val l0 = new java.util.ArrayList[Int](n)
      for (i ← Range(n))
        l0.add(i)
    }
    println(s"Vanilla Java ArrayList: $tArrayList")

    val t1 = conf measure {
      val l1 = ArrayBuffer.tabulate(n)(i => i)
    }
    println(s"Vanilla Scala ArrayBuffer: $t1")


    val t3 = conf measure {
      val pcmArraySeq = mut.ArraySeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ArraySeq: $t3")

    val tjll = conf measure {
      val tjll = new java.util.LinkedList[Int]
      for (i ← Range(n))
        tjll.add(i)
    }
    println(s"Vanilla Java LinkedList: $tjll")

    val t2 = conf measure {
      val l2 = ListBuffer.tabulate(n)(i => i)
    }
    println(s"Vanilla Scala ListBuffer: $t2")


    val t4 = conf measure {
      val l4 = mut.ListSeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ListSeq: $t4")

    val t5 = conf measure {
      val l5 = mut.ListBiSeq.tabulate(n)(i => i)
    }
    println(s"Poly-collection ListBiSeq: $t5")

    println()
  }

}
