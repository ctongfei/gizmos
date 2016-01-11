package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.ops._

/**
  * Represents a permutation on a set {0, ..., ''n'' - 1}.
  * @author Tongfei Chen
  * @since 0.1.0
  */
class Permutation private(private val data: Array[Int])
  extends BijectiveMap[Int, Int] with SortedMap[Int, Int] with HasKnownSize with TotalOrder[Int]
{
  override def size = data.length
  def apply(x: Int) = data(x)
  def invert(y: Int): Int = {
    for (k ← Range(size)) {
      if (data(k) == y) return k
    }
    throw new KeyNotFoundException(y)
  }

  def orderOnKey = TotalOrder[Int]
  def equivOnValue = Equiv[Int]

  def containsKey(x: Int) = x >= 0 && x < size
  def containsValue(y: Int) = containsKey(y)

  def ?(x: Int) = if (x < 0 || x >= size) None else Some(data(x))
  def invertOption(y: Int) = if (y < 0 || y >= size) None else Some(invert(y))

  def pairs = arrayAsIndexedSeq(data).pairs

  def compose(that: Permutation) = {
    require(this.size == that.size)
    new Permutation(Array.tabulate(size)(i => that(this(i))))
  }

  def cmp(x: Int, y: Int) = invert(x) >?< invert(y)

  override def inverse: Permutation = {
    val inv = Array.ofDim[Int](size)
    for (k ← Range(size))
      inv(data(k)) = k
    new Permutation(inv)
  }

  override def toString = arrayAsIndexedSeq(data).toString
}

object Permutation {

  def apply(ys: Int*): Permutation = {
    new Permutation(ys.toArray)
  }

  def identity(n: Int) = {
    val a = Array.tabulate(n)(i => i)
    new Permutation(a)
  }

  /**
    * Returns the permutation group of size ''n''.
    * This structure is a [[poly.algebra.Group]] as well as a [[poly.collection.Set]].
    *
    * @return
    */
  def Group(n: Int): Group[Permutation] with Set[Permutation] = new Group[Permutation] with Set[Permutation] {
    def inv(x: Permutation) = x.inverse
    def id = identity(n)
    def op(x: Permutation, y: Permutation) = x compose y
    def equivOnKey = LexicographicalOrder
    def elements = ??? // Generate all permutations based on lexicographical order
    def contains(x: Permutation) = x.size == n
  }

  implicit object LexicographicalOrder extends TotalOrder[Permutation] {
    def cmp(x: Permutation, y: Permutation): Int = {
      for (i ← Range(x.size)) {
        if (x(i) < y(i)) return -1
        if (x(i) > y(i)) return 1
      }
      0
    }
  }

}