package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.macroutil._

/**
 * Represents a permutation on the set '''Z''',,''n'',, = {0, ..., ''n'' - 1}.
 * <p>
 *   A permutation is a bijective endofunction on the set '''Z''',,''n'',, and defines
 *   a total order on the set '''Z''',,''n'',,. Its inverse (inverse of the bijective function)
 *   and its reverse (reverse of the total order) are also permutations on '''Z''',,''n'',,.
 * </p>
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Permutation private(private val a1: Array[Int], private val a2: Array[Int])
  extends BiMap[Int, Int] with IndexedSeq[Int] { self =>

  def fastLength = a1.length

  def fastApply(x: Int) = a1(x)

  def invert(y: Int) = a2(y)

  def invertOption(y: Int) = if (y < 0 || y >= size) None else Some(a2(y))

  override def keySet = Range(size).asSet

  override def valueSet = keySet

  def compose(that: Permutation) = {
    require(this.size == that.size)
    Permutation(Array.tabulate(size)(i => that(this(i))))
  }

  def andThen(that: Permutation) = that compose this

  /**
   * Casts this permutation as an order on the set '''Z''',,''n'',,.
   * @example Given a permutation (2, 0, 1), 2 < 0 < 1 holds under this order.
   */
  def asOrder: SequentialOrder[Int] with Bounded[Int] = new SequentialOrder[Int] with Bounded[Int] {
    def cmp(x: Int, y: Int) = a2(x) >?< a2(y)
    def pred(x: Int) = a1((a2(x) - 1) % size)
    def succ(x: Int) = a1((a2(x) + 1) % size)
    def top = a1(size - 1)
    def bot = a1(0)
  }

  /** Returns the inverse of this permutation. */
  override def inverse: Permutation = new Permutation(a2, a1)

  override def reverse: Permutation = {
    val n = size
    val b1 = Array.ofDim[Int](n)
    val b2 = Array.ofDim[Int](n)
    FastLoop.ascending(0, n, 1) { i =>
      b1(n - i - 1) = a1(i)
      b2(n - i - 1) = a2(i)
    }
    new Permutation(b1, b2)
  }

}

object Permutation {

  def apply(xs: Int*): Permutation = apply(xs.toArray)

  def apply(xs: Array[Int]): Permutation = {
    val ys = Array.ofDim[Int](xs.length)
    val bs = Array.fill(xs.length)(false)
    FastLoop.ascending(0, xs.length, 1) { i =>
      ys(xs(i)) = i
      bs(xs(i)) = true
    }
    // Requires that this is essentially a true bijection: all bits should be set as true
    require(bs forall {x => x})
    new Permutation(xs.clone, ys)
  }

  private[poly] def unchecked(xs: Array[Int]) = {
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

  def GroupAction[T](n: Int): GroupAction[IndexedSeq[T], Permutation] = new GroupAction[IndexedSeq[T], Permutation] {
    def actorGroup: Group[Permutation] = Group(n)
    def act(x: IndexedSeq[T], p: Permutation) = x permuteBy p
  }

  /**
    * Returns the permutation group of size ''n''.
    * This structure is a [[poly.algebra.Group]] as well as a [[poly.collection.Set]].
    */
  def Group(n: Int): Group[Permutation] with Set[Permutation] = new Group[Permutation] with Set[Permutation] {
    def inv(x: Permutation) = x.inverse
    def id = identity(n)
    def op(x: Permutation, y: Permutation) = x compose y
    def keyEq = LexicographicOrder
    def contains(x: Permutation) = x.size == n

    def keys: Iterable[Permutation] = Iterable.ofIterator {
      new AbstractIterator[Permutation] {
        private[this] var p: Array[Int] = null

        def current = Permutation.unchecked(p)

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

  //TODO: SequentialOrder
  implicit object LexicographicOrder extends Order[Permutation] {
    def cmp(x: Permutation, y: Permutation): Int = {
      FastLoop.ascending(0, x.size, 1) { i =>
        if (x(i) < y(i)) return -1
        if (x(i) > y(i)) return 1
      }
      0
    }
  }

}