package poly.collection.builder

import poly.collection._

/**
 * Represents a builder in which elements can be removed.
 * It can be used for estimation distributions from samples.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RemovableBuilder[-T, +C] extends Builder[T, C] { self =>

  def removeInplace(x: T): Unit

  def removeAllInplace(xs: Traversable[T]) = xs foreach removeInplace

  @inline final def -=(x: T) = removeInplace(x)

  @inline final def --=(xs: Traversable[T]) = xs foreach removeInplace

  override def map[D](f: C => D): RemovableBuilder[T, D] = new RemovableBuilderT.Mapped(self, f)

  override def |>[D](f: C => D) = map(f)

}

private[poly] object RemovableBuilderT {

  class Mapped[T, C, D](self: RemovableBuilder[T, C], f: C => D)
    extends BuilderT.Mapped[T, C, D](self, f) with RemovableBuilder[T, D]
  {
    def removeInplace(x: T) = self.removeInplace(x)
  }

}
