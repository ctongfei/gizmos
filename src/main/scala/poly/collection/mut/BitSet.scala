package poly.collection.mut

import poly.algebra._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class BitSet private(private final var data: Array[Long]) extends KeyMutableSet[Int] {

  def equivOnKey = Equiv.default[Int]

  def add(x: Int) = ???

  def remove(x: Int) = ???

  /** Tests if an element belongs to this set. */
  def contains(x: Int) = ???

  def keys = ???

  def clear() = ???
}
