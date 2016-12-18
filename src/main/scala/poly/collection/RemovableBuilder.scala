package poly.collection

/**
 * Represents a builder in which elements can be removed.
 * It can be used for estimating distributions from samples.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RemovableBuilder[-T, +R] extends Builder[T, R] { self =>

  def remove(x: T): Unit

  def removeAll(xs: Traversable[T]) = xs foreach remove

  @inline final def -=(x: T) = remove(x)

  @inline final def --=(xs: Traversable[T]) = removeAll(xs)

  override def map[D](f: R => D): RemovableBuilder[T, D] = new RemovableBuilderT.Mapped(self, f)

  override def contramap[S](f: S => T): Builder[S, R] = new RemovableBuilderT.Contramapped(self, f)

}

private[poly] object RemovableBuilderT {

  class Mapped[T, R, S](self: RemovableBuilder[T, R], f: R => S)
    extends BuilderT.Mapped[T, R, S](self, f) with RemovableBuilder[T, S]
  {
    def remove(x: T) = self.remove(x)
  }

  class Contramapped[S, T, R](self: RemovableBuilder[T, R], f: S => T)
    extends BuilderT.Contramapped[S, T, R](self, f) with RemovableBuilder[S, R]
  {
    def remove(x: S) = self remove f(x)
  }

}
