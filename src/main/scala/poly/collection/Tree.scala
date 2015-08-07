package poly.collection

import poly.util.typeclass._
import poly.util.typeclass.ops._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Tree[+T] {

  def root: T

  def children: Seq[Tree[T]]

}

object Tree {

  /**
   * Formats a tree into an S-expression.
   * @return An S-expression that represents the specific tree.
   */
  implicit def SexprFormatter[T: Formatter]: Formatter[Tree[T]] = new Formatter[Tree[T]] {
    def str(x: Tree[T]): String =
      "(" + x.root.str +
        (if (x.children.size == 0) "" else " ") +
        x.children.buildString(" ")(this) +
      ")"
  }
}
