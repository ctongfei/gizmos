package poly.collection.approx

import poly.algebra._
import poly.collection._
import poly.collection.impl.specialized._

/**
 * @author Tongfei Chen
 */
class CountMinSketch[K] private(val numBuckets: Int, private[this] val table: SpArrayTable[Int], val hashes: IndexedSeq[Hashing[K]]) extends Func[K, Int] {

  def add_!(x: K) = {
    var i = 0
    while (i < hashes.length) {
      table(i, hashes(i).hash(x) % numBuckets) += 1
      i += 1
    }
  }

  def +=(x: K) = add_!(x)

  def clear_!() = table.fillInplace(0)

  def weight(x: K) = {
    var min = Int.MaxValue
    var i = 0
    while (i < hashes.length) {
      val c = table(i, hashes(i).hash(x) % numBuckets)
      if (c < min) min = c
      i += 1
    }
    min
  }

  def apply(x: K) = weight(x)

}

object CountMinSketch {

  def apply[K](numBuckets: Int, hashes: IndexedSeq[Hashing[K]]) =
    new CountMinSketch[K](
      numBuckets,
      new SpArrayTable[Int](hashes.length, numBuckets),
      hashes
    )

}
