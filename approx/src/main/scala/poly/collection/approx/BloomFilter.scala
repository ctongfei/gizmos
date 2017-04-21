package poly.collection.approx

import poly.algebra._
import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class BloomFilter[K] private(val numBuckets: Int, private[this] val table: BitResizableArray, val hashes: IndexedSeq[Hashing[K]]) extends Func[K, Boolean] {

  def add_!(x: K) = { // explicit imperative style for speed
    var i = 0
    while (i < hashes.length) {
      table(hashes(i).hash(x) % numBuckets) = true
      i += 1
    }
  }

  def +=(x: K) = add_!(x)

  def clear_!() = table.clear()

  def contains(x: K): Boolean = {
    var i = 0
    while (i < hashes.length) {
      if (!table(hashes(i).hash(x) % numBuckets)) return false
      i += 1
    }
    true
  }

  def apply(x: K) = contains(x)

}

object BloomFilter {

  def apply[K](numBuckets: Int, hashes: IndexedSeq[Hashing[K]]) =
    new BloomFilter[K](
      numBuckets,
      new BitResizableArray(
        new Array[Long](Math.ceil(numBuckets.toDouble / BitResizableArray.LongSize).toInt)
      ),
      hashes
    )

}