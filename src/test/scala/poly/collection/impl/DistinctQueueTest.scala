package poly.collection.impl

import poly.algebra.syntax._
import poly.collection.mut._

/**
  * @author Tongfei Chen
  */
object DistinctQueueTest extends App {

  val a = DistinctQueue[ArrayStack, Int]()

  a push 0
  a push 3
  a push 2
  a push 0
  a pop()
  a pop()
  a push 0
  a push 3


}
