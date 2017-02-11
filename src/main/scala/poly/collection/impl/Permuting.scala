package poly.collection.impl

/**
 * @author Tongfei Chen
 */
private[poly] object Permuting {

  // Finds the rightmost i that satisfies p(i) < p(i+1).
  def findAscendingI(p: Array[Int]): Int = {
    var k = p.length - 2
    while (k >= 0) {
      if (p(k) < p(k + 1)) return k
      k -= 1
    }
    k
  }

  // Finds the least index j beyond i that p(j) > p(i).
  def findAscendingJ(p: Array[Int], i: Int): Int = {
    var l = i + 1
    var r = p.length - 1
    if (p(r) > p(i)) return r
    while (l < r) {
      val m = (l + r + 1) / 2
      if (p(m) > p(i)) l = m
      else r = m - 1
    }
    r
  }

  // Finds the rightmost i that satisfies p(i) > p(i+1).
  def findDescendingI(p: Array[Int]): Int = {
    var k = p.length - 2
    while (k >= 0) {
      if (p(k) > p(k + 1)) return k
      k -= 1
    }
    k
  }

  // Finds the least index j beyond i that p(j) < p(i).
  def findDescendingJ(p: Array[Int], i: Int): Int = {
    var l = i + 1
    var r = p.length - 1
    if (p(r) < p(i)) return r
    while (l < r) {
      val m = (l + r + 1) / 2
      if (p(m) < p(i)) l = m
      else r = m - 1
    }
    r
  }

  @inline def swap(p: Array[Int], i: Int, j: Int) = {
    val t = p(i)
    p(i) = p(j)
    p(j) = t
  }

  // Reverses the elements in left-inclusive-right-inclusive range [l, r].
  @inline def reverseBetween(p: Array[Int], i: Int, j: Int) = {
    var l = i
    var r = j
    while (l < r) {
      swap(p, l, r)
      l += 1
      r -= 1
    }
  }

  // Permutes the given array to its next permutation.
  // If it is the last permutation, permutes it to the first permutation and return false.
  def permuteToNext(p: Array[Int]): Boolean = {
    val n = p.length
    val i = findAscendingI(p)
    if (i == -1) {
      reverseBetween(p, 0, n - 1)
      return false
    }
    val j = findAscendingJ(p, i)
    swap(p, i, j)
    reverseBetween(p, i + 1, n - 1)
    true
  }

  // Permutes the given array to its previous permutation.
  // If it is the first permutation, permutes it to the last permutation and return false.
  def permuteToPrev(p: Array[Int]): Boolean = {
    val n = p.length
    val i = findDescendingI(p)
    if (i == -1) {
      reverseBetween(p, 0, n - 1)
      return false
    }
    val j = findDescendingJ(p, i)
    swap(p, i, j)
    reverseBetween(p, i + 1, n - 1)
    true
  }

}
