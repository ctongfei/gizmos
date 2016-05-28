package poly.collection

import poly.algebra._
import poly.algebra.syntax._

/**
 * Represents an unordered pair.
 * @since 0.1.0
 * @author Tongfei Chen
 */
case class UPair[+T](_1: T, _2: T) {

  override def equals(that: Any) = that match {
    case that: UPair[T] ⇒
      ((this._1 == that._1) && (this._2 == that._2)) ||
        ((this._1 == that._2) && (this._2 == that._1))
    case _ ⇒ false
  }

  override def hashCode = _1.## ^ _2.##

  override def toString = s"{${_1}, ${_2}}"

}

object UPair {

  class UPairEq[-T: Eq] extends Eq[UPair[T]] {
    def eq(x: UPair[T], y: UPair[T]) =
      ((x._1 === y._1) && (x._2 === y._2)) ||
        ((x._1 === y._2) && (x._2 === y._1))
  }
  implicit def Eq[T: Eq] = new UPairEq[T]

  implicit def Hashing[T: Hashing] = new UPairEq[T] with Hashing[UPair[T]] {
    def hash(x: UPair[T]) = x._1.### ^ x._2.###
  }

}