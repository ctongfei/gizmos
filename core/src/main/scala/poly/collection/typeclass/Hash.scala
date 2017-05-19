package poly.collection.typeclass

import poly.collection._
import poly.collection.specgroup._

/**
 * Represents an equivalence relation endowed with a hashing function.
 * This is the typeclass that a HashMap requires for its keys.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Hash[@sp X] extends Eq[X] with scala.util.hashing.Hashing[X] { self =>

  /** Returns the hash value of the given input using this hashing instance. */
  def hash(x: X): Int

  override def on[@sp Y](f: Y => X): Hash[Y] = new HashingT.Contramapped(self, f)

  def coproduct[Y](that: Hash[Y]): Hash[Either[X, Y]] = new HashingT.EitherHash(self, that)
}

object Hash {

  @inline def apply[T](implicit T: Hash[T]) = T

  implicit def anyVal[@sp T <: AnyVal]: Hash[T] = new Hash[T] {
    def hash(x: T): Int = x.##
    def eqv(x: T, y: T) = x == y
  }

  /** Creates a `IntHashing` object from the specific hash function. */
  def create[@specialized X](fHash: X => Int)(implicit X: Eq[X]): Hash[X] = new HashingT.OfHashFunc[X](fHash, X)

  def by[@specialized(Int) X, @specialized(Int) Y](f: Y => X)(implicit X: Hash[X]) = new HashingT.Contramapped(X, f)

  /** Creates an `Hashing` object from a type's inherent `hashCode`/`##` method. */
  def default[@specialized X]: Hash[X] = new HashingT.Default[X]

  /** Creates an `Hashing` object using the identity (by-reference) hashing function and identity equivalence relation. */
  def byRef[X <: AnyRef]: Hash[X] = new HashingT.ByRef[X]

  def onTuple1[@sp(spTuple1) A](implicit A: Hash[A]): Hash[Tuple1[A]] = new HashingT.Tuple1Hash(A)
  def onTuple2[@sp(spTuple2) A, @sp(spTuple2) B](implicit A: Hash[A], B: Hash[B]): Hash[(A, B)] = new HashingT.Tuple2Hash(A, B)
  def onTuple3[A, B, C](implicit A: Hash[A], B: Hash[B], C: Hash[C]): Hash[(A, B, C)] = new HashingT.Tuple3Hash(A, B, C)
  def onEither[A, B](implicit A: Hash[A], B: Hash[B]): Hash[Either[A, B]] = new HashingT.EitherHash(A, B)

}

abstract class AbstractHash[@sp X] extends Hash[X]

private[poly] object HashingT {
  class OfHashFunc[@sp(Int, Long, Float, Double) X](fHash: X ⇒ Int, X: Eq[X]) extends Hash[X] {
    def hash(x: X) = fHash(x)
    def eqv(x: X, y: X) = X.eqv(x, y)
  }

  class Contramapped[@sp X, Y](self: Hash[X], f: Y ⇒ X) extends Hash[Y] {
    def hash(y: Y) = self.hash(f(y))
    def eqv(x: Y, y: Y) = self.eqv(f(x), f(y))
  }

  class Default[@sp X] extends Hash[X] {
    def hash(x: X) = x.##
    def eqv(x: X, y: X) = x == y
  }

  class ByRef[X <: AnyRef] extends AbstractHash[X] {
    def hash(x: X) = System.identityHashCode(x)
    def eqv(x: X, y: X) = x eq y
  }

  // not inheriting EqT.Tuple1Eq because specialization wouldn't work
  class Tuple1Hash[@sp(spTuple1) A](A: Hash[A]) extends Hash[Tuple1[A]] {
    def hash(x: Tuple1[A]) = Tuple1(A.hash(x._1)).##
    def eqv(x: Tuple1[A], y: Tuple1[A]) = A.eqv(x._1, y._1)
  }

  // not inheriting EqT.Tuple2Eq because specialization wouldn't work
  class Tuple2Hash[@sp(spTuple2) A, @sp(spTuple2) B](A: Hash[A], B: Hash[B]) extends Hash[(A, B)] {
    def hash(x: (A, B)) = (A.hash(x._1), B.hash(x._2)).##
    def eqv(x: (A, B), y: (A, B)) = A.eqv(x._1, y._1) && B.eqv(x._2, y._2)
  }

  class Tuple3Hash[A, B, C](A: Hash[A], B: Hash[B], C: Hash[C]) extends Hash[(A, B, C)] {
    def eqv(x: (A, B, C), y: (A, B, C)): Boolean = A.eqv(x._1, y._1) && B.eqv(x._2, y._2) && C.eqv(x._3, y._3)
    def hash(x: (A, B, C)) = (A.hash(x._1), B.hash(x._2), C.hash(x._3)).##
  }

  class EitherHash[A, B](A: Hash[A], B: Hash[B]) extends Hash[Either[A, B]] {
    def hash(x: Either[A, B]) = x match {
      case Left(l)  => Left(A.hash(l)).##
      case Right(r) => Right(B.hash(r)).##
    }
    override def eqv(x: Either[A, B], y: Either[A, B]): Boolean = x match {
      case Left(xl) => y match {
        case Left(yl) => A.eqv(xl, yl)
        case _ => false
      }
      case Right(xr) => y match {
        case Right(yr) => B.eqv(xr, yr)
        case _ => false
      }
    }
  }

}