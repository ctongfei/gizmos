package poly.collection

import poly.algebra._
import poly.algebra.specgroup._
import poly.algebra.syntax._
import poly.macroutil._
import scala.reflect.macros.blackbox._
import scala.language.experimental.macros

/**
 * Represents an immutable integer range.
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed trait Range extends SortedIndexedSeq[Int] { self ⇒

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

  @inline final override def foreach[@sp(Unit) V](f: Int ⇒ V): Unit = {
    var i = left
    while (i <= last) {
      f(i)
      i += step
    }
  }

  def sum = (head + last) * length / 2

  def asSet: SortedSet[Int] = new AbstractSortedSet[Int] {
    override def size = self.fastLength
    def orderOnKeys = self.orderOnElements
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
      def foreach[U](f: Int ⇒ U): Unit = macro ascendingForeachMacroImpl[U]
    }

    def fast = new FastTraversable(this)

    def intersect(that: Range.Ascending) = {
      val right = function.min(this.right, that.right)
      val step = lcm(this.step, that.step)
      if ((this.left - that.left) % gcd(this.step, that.step) != 0)
        new Range.Ascending(this.right, that.right, step) // empty range if (this.left != that.left) mod gcd
      else {
        val r1 = if (this.left < that.left) that else this
        val r0 = if (this.left < that.left) this else that
        var left = r1.left
        while ((left - r0.left) % r0.step != 0) left += r1.step // find one special solution for the Diophantine equation
        new Range.Ascending(left, right, step)
      }
    }

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
      def foreach[U](f: Int ⇒ U): Unit = macro descendingForeachMacroImpl[U]
    }

    def fast = new FastTraversable(this)
    def orderOnElements = Order[Int].reverse
    def intersect(that: Range.Descending) = (this.reverse intersect that.reverse).reverse
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

  def ascendingForeachMacroImpl[V](c: Context)(f: c.Expr[Int ⇒ V]): c.Expr[Unit] = {
    import c.universe._
    val i = TermName(c.freshName("i"))
    val range = TermName(c.freshName("range"))
    val limit = TermName(c.freshName("limit"))
    val step = TermName(c.freshName("step"))
    val tree = c.macroApplication match {
      case q"$r.fast.foreach[$ty]($f)" ⇒
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

  def descendingForeachMacroImpl[V](c: Context)(f: c.Expr[Int ⇒ V]): c.Expr[Unit] = {
    import c.universe._
    val i = TermName(c.freshName("i"))
    val range = TermName(c.freshName("range"))
    val limit = TermName(c.freshName("limit"))
    val step = TermName(c.freshName("step"))
    val tree = c.macroApplication match {
      case q"$r.fast.foreach[$ty]($f)" ⇒
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
