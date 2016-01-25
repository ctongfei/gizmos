package poly.collection.benchmark

import org.scalameter._
import poly.collection.impl._
import scala.collection.{mutable => scm}
import java.{util => ju}
import poly.algebra.syntax._
import poly.collection.{mut => pcm}
import scala.util._

/**
  * @author Tongfei Chen
  */
object PriorityQueueBenchmark extends App {

  val conf = config(Key.exec.benchRuns → 50)
      .withWarmer(new Warmer.Default).withMeasurer(new Measurer.Default)

  val r = new Random()

  for (n ← scala.Seq(100000, 200000, 400000, 800000, 1600000, 3200000)) {

    val nums = Array.fill(n)(r.nextInt())

    val tScala = conf measure {
      val pq = scm.PriorityQueue[Int]()
      nums foreach pq.+=
    }
    print(s"Scala PriorityQueue: $tScala")

    val tJava = conf measure {
      val pq = new ju.PriorityQueue[Int]()
      nums foreach pq.add
    }
    print(s"\t\tJava PriorityQueue: $tJava")

    val tPoly = conf measure {
      val pq = pcm.BinaryHeap[Int]()
      nums foreach pq.push
    }
    print(s"\t\tPoly BinaryHeap: $tPoly")
    println()
  }

}
