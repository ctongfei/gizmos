package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.macroutil._

/**
 * Represents a permutation on a set N,,''n'',, = {0, ..., ''n'' - 1}.
 * <p>
 *   A permutation is a bijective endofunction on the set N,,''n'',, and defines
 *   a total order on the set N,,''n'',,. Its inverse (inverse of the bijective function)
 *   and its reverse (reverse of the total order) are also permutations on N,,''n'',,.
 * </p>
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Permutation private(private val a1: Array[Int], private val a2: Array[Int])
  extends BijectiveMap[Int, Int] with IndexedSeq[Int] with SequentialOrder[Int] with Bounded[Int] with HasKnownSize
{
  def fastLength = a1.length
  def fastApply(x: Int) = a1(x)
  def invert(y: Int) = a2(y)

  override def size = a1.length

  def equivOnValue = Equiv[Int]

  def containsValue(y: Int) = containsKey(y)

  def invertOption(y: Int) = if (y < 0 || y >= size) None else Some(a2(y))

  def compose(that: Permutation) = {
    require(this.size == that.size)
    Permutation(Array.tabulate(size)(i => that(this(i))))
  }

  def andThen(that: Permutation) = that compose this

  def cmp(x: Int, y: Int) = a2(x) >?< a1(y)
  def pred(x: Int) = a1((a2(x) - 1) % size)
  def succ(x: Int) = a1((a2(x) + 1) % size)
  def top = a1(size - 1)
  def bot = a1(0)

  override def inverse: Permutation = new Permutation(a2, a1)

  override def reverse: Permutation = {
    val n = length
    val b1 = Array.ofDim[Int](n)
    val b2 = Array.ofDim[Int](n)
    FastLoop.ascending(0, n, 1) { i =>
      b1(n - i) = a1(i)
      b2(n - i) = a2(i)
    }
    new Permutation(b1, b2)
  }

  override def toString = arrayAsIndexedSeq(a1).toString
}

object Permutation {

  def apply(xs: Int*): Permutation = apply(xs.toArray)

  def apply(xs: Array[Int]): Permutation = {
    val ys = Array.ofDim[Int](xs.length)
    FastLoop.ascending(0, xs.length, 1) { i =>
      ys(xs(i)) = i
    }
    new Permutation(xs.clone, ys)
  }

  /** Generates a random permutation of the given length. */
  def random(n: Int) = {
    val a = Array.tabulate(n)(i => i)
    val r = new java.util.Random()
    FastLoop.descending(n - 1, 0, -1) { i =>
      val j = r.nextInt(i + 1)
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    Permutation(a)
  }

  /** Generates an identity permutation of the given length. */
  def identity(n: Int) = {
    val a = Array.tabulate(n)(i => i)
    new Permutation(a, a)
  }

  implicit def GroupAction[T]: Action[IndexedSeq[T], Permutation] = new Action[IndexedSeq[T], Permutation] {
    def act(p: Permutation, x: IndexedSeq[T]) = x permuteBy p
  }

  /**
    * Returns the permutation group of size ''n''.
    * This structure is a [[poly.algebra.Group]] as well as a [[poly.collection.Set]].
    */
  def Group(n: Int): Group[Permutation] with Set[Permutation] = new Group[Permutation] with Set[Permutation] {
    def inv(x: Permutation) = x.inverse
    def id = identity(n)
    def op(x: Permutation, y: Permutation) = x compose y
    def equivOnKey = LexicographicOrder
    def contains(x: Permutation) = x.size == n

    def keys: Iterable[Permutation] = Iterable.ofIterator {
      new Iterator[Permutation] {
        var p: Array[Int] = null

        def current = Permutation(p)

        // Generate permutations under lexicographic order.
        def advance(): Boolean = {
          if (p == null) {
            p = Array.tabulate(n)(i => i)
            true
          }
          else {
            var k = -1
            FastLoop.ascending(0, n - 1, 1) { i => if (p(i) < p(i + 1)) k = i }
            if (k == -1) return false
            var l = k + 1
            FastLoop.ascending(k + 1, n, 1) { i => if (p(k) < p(i)) l = i }
            val t = p(k)
            p(k) = p(l)
            p(l) = t
            var left = k + 1
            var right = n - 1
            while (left < right) {
              val t = p(right)
              p(right) = p(left)
              p(left) = t
              left += 1
              right -= 1
            }
            true
          }
        }
      }
    }
  }

  implicit object LexicographicOrder extends SequentialOrder[Permutation] {
    def cmp(x: Permutation, y: Permutation): Int = {
      FastLoop.ascending(0, x.size, 1) { i =>
        if (x(i) < y(i)) return -1
        if (x(i) > y(i)) return 1
      }
      0
    }

    def pred(x: Permutation) = ???

    def succ(x: Permutation) = ???
  }

}