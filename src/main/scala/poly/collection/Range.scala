package poly.collection

import poly.algebra._
import poly.algebra.specgroup._
import poly.algebra.syntax._
import poly.macroutil._

import scala.reflect.macros.blackbox._
import scala.language.experimental.macros

/**
 * Represents an immutable integer range.
 *
 * The difference between this class and [[scala.collection.immutable.Range]] is that
 * [[poly.collection.Range]] will attempt to inline the loop body when iterating over
 * the range using macros, which potentially makes it more efficient.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed trait Range extends SortedIndexedSeq[Int] { self =>

  def left: Int

  def right: Int

  def step: Int
  
  val fastLength = {
    val gap = right - left
    val len = gap / step + (if (gap % step != 0) 1 else 0)
    if (len < 0) 0 else len
  }
  def fastApply(i: Int): Int = left + i * step

  override val head = left

  override val last = left + (fastLength - 1) * step

  // HELPER FUNCTIONS

  @inline final override def foreach[@sp(Unit) V](f: Int => V): Unit = {
    var i = left
    while (true) {
      f(i)
      if (i == last) return
      i += step
    }
  }

  def sum = (head + last) * length / 2

  def asSet: Set[Int] = new AbstractSet[Int] {
    override def size = self.fastLength
    def eqOnKeys = self.orderOnElements
    def keys = self
    def contains(x: Int) =
      if (step > 0) x >= left && x < right && (x - left) % step == 0
      else x <= left && x > right && (left - x) % (-step) == 0
  }
}

object Range {

  final class Ascending(
    val left: Int,
    val right: Int,
    val step: Int = 1
  ) extends Range { require(step > 0)

    class FastTraversable(r: Range.Ascending) {
      def foreach[U](f: Int => U) = macro ascendingForeachMacroImpl[U]
    }

    def fast = new FastTraversable(this)

    def orderOnElements = Order[Int]
    override def tail = new Range.Ascending(left + step, right, step)
    override def reverse = new Range.Descending(left + step * (length - 1), left - math.signum(step), -step)
  }

  final class Descending(
    val left: Int,
    val right: Int,
    val step: Int = 1
  ) extends Range { require(step < 0)

    class FastTraversable(r: Range.Descending) {
      def foreach[U](f: Int => U) = macro descendingForeachMacroImpl[U]
    }

    def fast = new FastTraversable(this)
    def orderOnElements = Order[Int].reverse
    override def tail = new Range.Descending(left + step, right, step)
    override def reverse = new Range.Ascending(left + step * (length - 1), left - math.signum(step), -step)
  }

  /** Creates a left-inclusive-right-exclusive range [0, ''r''). */
  def apply(r: Int) = new Range.Ascending(0, r)

  /** Creates a left-inclusive-right-exclusive range [''l'', ''r''). */
  def apply(l: Int, r: Int) = new Range.Ascending(l, r)

  /** Creates a left-inclusive-right-exclusive range with the specific step size (can be negative). */
  def apply(l: Int, r: Int, step: Int): Range = {
    if (step > 0) new Range.Ascending(l, r, step)
    else new Range.Descending(l, r, step)
  }

  /** Creates a closed range [0, ''r'']. */
  def inclusive(r: Int) = new Range.Ascending(0, r + 1)

  /** Creates a closed range [''l'', ''r'']. */
  def inclusive(l: Int, r: Int) = new Range.Ascending(l, r + 1)

  /** Creates a closed range with the specific step size (can be negative). */
  def inclusive(l: Int, r: Int, step: Int): Range = {
    if (step > 0) new Range.Ascending(l, r + math.signum(step), step)
    else new Range.Descending(l, r + math.signum(step), step)
  }

  def ascendingForeachMacroImpl[V](c: Context)(f: c.Expr[Int => V]): c.Expr[Unit] = {
    import c.universe._
    val i = TermName(c.freshName("i"))
    val range = TermName(c.freshName("range"))
    val limit = TermName(c.freshName("limit"))
    val step = TermName(c.freshName("step"))
    val tree = c.macroApplication match {
      case q"$r.fast.foreach[$ty]($f)" =>
        q"""
          val $range = $r
          var $i = $range.left
          val $limit = $range.right
          val $step = $range.step
          while ($i < $limit) {
            $f($i)
            $i += $step
          }
        """
    }
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def descendingForeachMacroImpl[V](c: Context)(f: c.Expr[Int => V]): c.Expr[Unit] = {
    import c.universe._
    val i = TermName(c.freshName("i"))
    val range = TermName(c.freshName("range"))
    val limit = TermName(c.freshName("limit"))
    val step = TermName(c.freshName("step"))
    val tree = c.macroApplication match {
      case q"$r.fast.foreach[$ty]($f)" =>
        q"""
          val $range = $r
          var $i = $range.left
          val $limit = $range.right
          val $step = $range.step
          while ($i > $limit) {
            $f($i)
            $i += $range.step
          }
        """
    }
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

}
