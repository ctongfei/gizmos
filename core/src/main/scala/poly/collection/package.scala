package poly

import cats.kernel._
import poly.collection.conversion._
import poly.collection.conversion.FromScala._
import poly.collection.mut._

import scala.language.higherKinds
import scala.language.implicitConversions

package object collection extends ImplicitOps {

  type <=>[A, B] = Bijection[A, B]

  /** Returns the first element of two elements. */
  //@inline def first[@sp(di) α, @sp(di) β](a: α, b: β) = a

  /** Returns the second element of two elements. */
  //@inline def second[@sp(di) α, @sp(di) β](a: α, b: β) = b

  /** Returns the first element of a pair. */
  scala.Tuple2
  @inline def first[@specialized(Int, Long, Double, Char, Boolean) α, @specialized(Int, Long, Double, Char, Boolean) β]
  (pair: (α, β)) = pair._1

  /** Returns the second element of a pair. */
  @inline def second[@specialized(Int, Long, Double, Char, Boolean) α, @specialized(Int, Long, Double, Char, Boolean) β]
  (pair: (α, β)) = pair._2

  @inline def third[γ](triple: (_, _, γ)) = triple._3

  @inline private[poly] def hashByRef[A <: AnyRef](x: A) = System.identityHashCode(x)

  @inline private[poly] def default[T]: T = null.asInstanceOf[T]

  @inline private[poly] def zero[T](implicit T: AdditiveMonoid[T]) = T.zero

  private[poly] implicit def eqTuple2[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] = new Eq[(A, B)] {
    def eqv(x: (A, B), y: (A, B)) = A.eqv(x._1, y._1) && B.eqv(x._2, y._2)
  }

  private[poly] implicit class javaComparatorAsCatsOrder[U, T <: U](val o: java.util.Comparator[U]) extends Order[T] {
    def compare(x: T, y: T): Int = o.compare(x, y)
  }
  private[poly] implicit class scalaOrderingAsCatsOrder[U](val o: scala.math.Ordering[U]) extends Order[U] {
    def compare(x: U, y: U): Int = o.compare(x, y)
  }
  
  private[poly] def nextPowerOfTwo(x: Int): Int = {
    var c = x - 1
    c |= c >>> 1
    c |= c >>> 2
    c |= c >>> 4
    c |= c >>> 8
    c |= c >>> 16
    c + 1
  }

  private[poly] def nextHalfPowerOfTwo(x: Int): Int = {
    val p = nextPowerOfTwo(x)
    val q = p / 2
    if (q > x) q else p
  }

  private object GetStringValue {
    lazy val field = {
      val f = classOf[String].getDeclaredField("value")
      f.setAccessible(true)
      f
    }
  }

  /**
   * Gets the underlying `Array[T]` from a varargs argument of type `T*`.
   */
  private[poly] def getArrayFromVarargs[T](xs: scala.Seq[T]): Array[T] = xs match {
    case xs: scala.collection.mutable.WrappedArray[T] => xs.array
    case _ => xs.asPoly.to(ArraySeq).data.data.asInstanceOf[Array[T]] // this cast is safe!
  }

  /**
   * Gets the underlying `Array[Char]` from a String.
   * @note This method uses Java reflection.
   */
  private[poly] def getArrayFromString(s: String): Array[Char] = {
    GetStringValue.field.get(s).asInstanceOf[Array[Char]]
  }

  private[poly] implicit class WithModOps(val x: Int) extends AnyVal {
    def %+(mod: Int) = {
      val r = x % mod
      if (r >= 0) r else r + mod
    }
  }

  private[poly] implicit class EqOps[T](val T: Eq[T]) extends AnyVal {
    def product[U](U: Eq[U]): Eq[(T, U)] = new Eq[(T, U)] {
      def eqv(x: (T, U), y: (T, U)) = T.eqv(x._1, y._1) && U.eqv(x._2, y._2)
    }
  }

  private[poly] implicit class OrderOps[T](val T: Order[T]) extends AnyVal {
    def refine[U <: T]: Order[U] = new Order[U] {
      def compare(x: U, y: U): Int = T.compare(x, y)
    }
  }

  implicit class HashingOps[T](val t: T) extends AnyVal {
    def ###(implicit h: Hashing[T]) = h hash t
  }

  implicit def arrayAsPoly[T](a: Array[T]): IndexedSeq[T] = new ArrayAsPoly[T](a)
  implicit def charSequenceAsPoly(s: CharSequence): IndexedSeq[Char] = new JavaCharSequenceAsPoly(s)
  implicit def booleanFunctionAsPoly[T](f: T => Boolean): Predicate[T] = new BooleanFunctionAsPredicate[T](f)
  implicit def stringBuilderAsPoly(sb: StringBuilder): Builder[Char, String] = new ScalaStringBuilderAsBuilder(sb)
  implicit def javaStringBuilderAsPoly(sb: java.lang.StringBuilder): Builder[Char, String] = new JavaStringBuilderAsPoly(sb)


  private[collection] type Eq[T]              = cats.kernel.Eq[T]
  private[collection] val  Eq                 = cats.kernel.Eq

  private[collection] type PartialOrder[T]    = cats.kernel.PartialOrder[T]

  private[collection] type Order[T]           = cats.kernel.Order[T]
  private[collection] val  Order              = cats.kernel.Order

  private[collection] type Semigroup[T]       = cats.kernel.Semigroup[T]
  private[collection] type Monoid[T]          = cats.kernel.Monoid[T]
  private[collection] type Group[T]           = cats.kernel.Group[T]

  private[collection] type AdditiveMonoid[T]  = algebra.ring.AdditiveMonoid[T]
  private[collection] type AdditiveGroup[T]   = algebra.ring.AdditiveGroup[T]
  private[collection] type Ring[T]            = algebra.ring.Ring[T]
  private[collection] type Lattice[T]         = algebra.lattice.Lattice[T]
  private[collection] type Bool[T]            = algebra.lattice.Bool[T]

  private[collection] type Field[T]           = spire.algebra.Field[T]
  private[collection] type Module[T, R]       = spire.algebra.Module[T, R]
  private[collection] type Action[T, R]       = spire.algebra.Action[T, R]
  private[collection] type MetricSpace[T, F]  = spire.algebra.MetricSpace[T, F]
  private[collection] type VectorSpace[T, F]  = spire.algebra.VectorSpace[T, F]

  private[collection] type Hashing[T]         = poly.collection.typeclass.Hashing[T]
  private[collection] val  Hashing            = poly.collection.typeclass.Hashing

  private[collection] type Cloning[T]         = poly.collection.typeclass.Cloning[T]
  private[collection] val  Cloning            = poly.collection.typeclass.Cloning

  private[collection] type SequentialOrder[T] = poly.collection.typeclass.SequentialOrder[T]

  private[collection] type Id[T]              = cats.Id[T]
  private[collection] type Functor[F[_]]      = cats.Functor[F]
  private[collection] type Applicative[F[_]]  = cats.Applicative[F]
  private[collection] type Monad[F[_]]        = cats.Monad[F]
  private[collection] type MonadCombine[F[_]] = cats.MonadCombine[F]
  private[collection] type Comonad[F[_]]      = cats.Comonad[F]
  private[collection] type Category[->[_, _]] = cats.arrow.Category[->]
  private[collection] type Arrow[->[_, _]]    = cats.arrow.Arrow[->]

}
