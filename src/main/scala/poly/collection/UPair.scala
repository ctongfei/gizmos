package poly.collection

import poly.algebra._
import poly.algebra.specgroup._
import poly.algebra.syntax._
import poly.collection.immut._

/**
 * Represents an unordered pair.
 * @since 0.1.0
 * @author Tongfei Chen
 */
case class UPair[@sp(spTuple2) T: Eq](_1: T, _2: T) extends Set[T] {

  def contains(x: T) = x === _1 || x === _2

  def keys = List(_1, _2)

  def keyEq = Eq[T]

  override final def size = 2

  override def equals(that: Any) = that match {
    case that: UPair[T] =>
      ((this._1 == that._1) && (this._2 == that._2)) ||
        ((this._1 == that._2) && (this._2 == that._1))
    case _ => false
  }

  override def hashCode = {
    val a = _1.##
    val b = _2.##
    (a * b) + (a ^ b) // commutative!
  }

  override def toString = s"{${_1}, ${_2}}"

}

object UPair {

  class UPairEq[@sp(spTuple2) T: Eq] extends Eq[UPair[T]] {
    def eq(x: UPair[T], y: UPair[T]) =
      ((x._1 === y._1) && (x._2 === y._2)) ||
        ((x._1 === y._2) && (x._2 === y._1))
  }

  // Does not inherit UPairEq because of specialization issues
  class UPairHashing[@sp(spTuple2) T: Hashing] extends Hashing[UPair[T]] {
    def eq(x: UPair[T], y: UPair[T]) =
      ((x._1 === y._1) && (x._2 === y._2)) ||
        ((x._1 === y._2) && (x._2 === y._1))
    def hash(x: UPair[T]) = x._1.### ^ x._2.###
  }

  implicit def Eq[@sp(spTuple2) T](implicit T: Eq[T]): Eq[UPair[T]] = T match {
    case ht: Hashing[T] => new UPairHashing[T]()(ht)
    case _ => new UPairEq[T]()(T)
  }

  def Hashing[@sp(spTuple2) T](implicit T: Hashing[T]): Hashing[UPair[T]] = new UPairHashing()(T)

}
