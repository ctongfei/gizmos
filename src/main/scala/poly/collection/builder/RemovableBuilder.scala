package poly.collection.builder

import poly.collection._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RemovableBuilder[-T, +C] extends Builder[T, C] { self ⇒

  def removeInplace(x: T): Unit

  def removeAllInplace(xs: Traversable[T]) = xs foreach removeInplace

  @inline final def -=(x: T) = removeInplace(x)

  @inline final def --=(xs: Traversable[T]) = xs foreach removeInplace

  override def map[D](f: C ⇒ D): RemovableBuilder[T, D] = new RemovableBuilder[T, D] {
    def addInplace(x: T) = self.addInplace(x)
    def removeInplace(x: T) = self.removeInplace(x)
    def result = f(self.result)
}

}
