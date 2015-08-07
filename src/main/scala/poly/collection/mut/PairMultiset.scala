package poly.collection.mut

import poly.collection.factory._
import poly.collection.impl._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class PairMultiset[T, C[T1] <: Set[T1]] private() extends Multiset[T] {

  val data: C[KeyValuePair[T, Int]] = null

  def contains(x: T): Boolean = ???

  def size: Int = ???

  def multiplicity(x: T): Int = ???

  def elements: Enumerable[T] = ???

}

