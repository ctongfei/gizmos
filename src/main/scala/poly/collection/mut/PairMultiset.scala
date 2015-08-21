package poly.collection.mut

import poly.collection.factory._
import poly.collection.impl._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class PairMultiset[T, C[A] <: Set[A]] private()(implicit builder: Builder[T, C[T]]) extends Multiset[T] {

  val data: C[KeyValuePair[T, Int]]

  def contains(x: T): Boolean = ???

  def size: Int = ???

  def multiplicity(x: T): Int = ???

  def elements: Enumerable[T] = ???

}

