package poly.collection

import cats.arrow._

/**
 * Represents an one-to-one (bijective) function between two types.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Bijection[@specialized(Int, AnyRef) X, @specialized(Int, AnyRef) Y] extends Func[X, Y] { self =>

  /** Given a value ''x'' in the domain, returns the unique value ''y'' in the codomain that ''x'' maps to. */
  def apply(x: X): Y

  /** Given a value ''y'' in the codomain, returns the unique value ''x'' in the domain that maps to ''y''. */
  def invert(y: Y): X

  /** Returns the inverse bijection of this bijection. */
  def inverse: Bijection[Y, X] = new BijectionT.Inverse(self)

  /** Composes two bijections with this bijection applied first. */
  def andThen[Z](that: Bijection[Y, Z]): Bijection[X, Z] = new BijectionT.Composed(that, self)

  /** Composes two bijections with this bijection applied last. */
  def compose[W](that: Bijection[W, X]): Bijection[W, Y] = new BijectionT.Composed(self, that)

  /** Returns the Cartesian product of two bijections. */
  def product[U, V](that: Bijection[U, V]): Bijection[(X, U), (Y, V)] = new BijectionT.Product(self, that)

  def map[Z](f: Bijection[Y, Z]): Bijection[X, Z] = andThen(f)

  def contramap[W](f: Bijection[W, X]): Bijection[W, Y] = compose(f)

  final def ∘[W](that: Bijection[W, X]): Bijection[W, Y] = compose(that)
  final def ×[U, V](that: Bijection[U, V]): Bijection[(X, U), (Y, V)] = product(that)

  def unapply(y: Y): Option[X] = Some(invert(y))

}

object Bijection {

  def apply[X, Y](f1: X => Y, f2: Y => X): X <=> Y = new AbstractBijection[X, Y] {
    def invert(y: Y) = f2(y)
    def apply(x: X): Y = f1(x)
  }

  /** Returns the identity bijection. */
  def identity[X]: X <=> X = new BijectionT.Identity[X]

  implicit object Category extends Category[Bijection] {
    def id[X] = Bijection(x => x, x => x)
    def compose[X, Y, Z](g: Y <=> Z, f: X <=> Y) = g compose f
  }

}

abstract class AbstractBijection[@specialized(Int, AnyRef) X, @specialized(Int, AnyRef) Y] extends Bijection[X, Y]

private[poly] object BijectionT {

  class Identity[@specialized(Int, AnyRef) X] extends Bijection[X, X] {
    def invert(x: X) = x
    def apply(x: X) = x
    override def inverse = this
  }

  class Inverse[@specialized(Int, AnyRef) X, @specialized(Int, AnyRef) Y](self: Bijection[X, Y]) extends Bijection[Y, X] {
    def invert(x: X) = self.apply(x)
    def apply(y: Y) = self.invert(y)
    override def inverse = self
  }

  class Composed[W, X, Y](self: Bijection[X, Y], that: Bijection[W, X]) extends AbstractBijection[W, Y] {
    def invert(y: Y) = that.invert(self.invert(y))
    def apply(w: W) = self(that(w))
  }

  class Product[X, Y, U, V](self: Bijection[X, Y], that: Bijection[U, V]) extends AbstractBijection[(X, U), (Y, V)] {
    def invert(yv: (Y, V)) = (self.invert(yv._1), that.invert(yv._2))
    def apply(xu: (X, U)) = (self(xu._1), that(xu._2))
  }

}
