package poly.collection.impl.hashtable

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class OpenHashTable[K: IntHashing, E >: Null <: OpenHashEntryLike[K, E]](initialSize: Int = Settings.ArrayInitialSize) {

  var table: Array[AnyRef] = Array.ofDim[AnyRef](initialSize)
  var size: Int = 0
  var threshold = (table.length * Settings.HashTableLoadFactor).toInt

  def index(h: Int): Int = {
    val ones = table.length - 1
    val improved = improve(h, Integer.bitCount(table.length - 1))
    val shifted = (improved >> (32 - Integer.bitCount(ones))) & ones
    shifted
  }

  def improve(h: Int, seed: Int): Int = {
    val i = scala.util.hashing.byteswap32(h)
    val rotation = seed % 32
    val rotated = (i >>> rotation) | (i << (32 - rotation))
    rotated
  }

  def locate(k: K): E = {
    val h = index(k.###)
    var e = table(h).asInstanceOf[E]
    while (e != null && (e.key !== k))
      e = e.nextEntry
    e
  }

  def addEntry(e: E) = {
    val h = index(e.key.###)
    e.nextEntry = table(h).asInstanceOf[E]
    table(h) = e
    size += 1
    if (size > threshold)
      grow(table.length * 2)
  }

  def removeEntry(k: K) = {
    val h = index(k.###)
    var e = table(h).asInstanceOf[E]
    if (e != null) {
      if (k === e.key) {
        table(h) = e.nextEntry
        size -= 1
      } else {
        var ee = e.nextEntry
        while (ee != null && (ee.key !== k)) {
          e = ee
          ee = ee.nextEntry
        }
        if (ee != null) {
          e.nextEntry = ee.nextEntry
          size -= 1
        }
      }
    }
  }

  def entryIterator: Iterator[E] = new Iterator[E] {
    private[this] var i = -1
    private[this] var e: E = null
    def current = e
    def advance(): Boolean = {
      if (e != null && e.nextEntry != null) {
        e = e.nextEntry
        true
      } else {
        while (true) {
          i += 1
          if (i >= table.length) return false
          if (table(i) != null) {
            e = table(i).asInstanceOf[E]
            return true
          }
        }
        false
      }
    }
  }

  def foreachEntry[U](f: E => U) = {
    var i = 0
    var e: E = null
    for (i ← Range(table.length)) {
      e = table(i).asInstanceOf[E]
      while (e != null) {
        f(e)
        e = e.nextEntry
      }
    }
  }

  def clear() = {
    for (i ← Range(table.length))
      table(i) = null
    size = 0
  }

  def grow(newSize: Int) = {
    val old = table
    table = new Array[AnyRef](nextPowerOfTwo(newSize))
    for (i ← Range(old.length)) {
    var e = old(i).asInstanceOf[E]
      while (e != null) {
        val h = index(e.key.###)
        val ee = e.nextEntry
        e.nextEntry = table(h).asInstanceOf[E]
        table(h) = e
        e = ee
      }
    }
    threshold = (table.length * Settings.HashTableLoadFactor).toInt
  }
}
