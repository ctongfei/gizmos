package poly.collection.impl

import poly.algebra._
import poly.algebra.ops._
import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class HashTable[T](implicit h: Hashing[T, Int]) {

  final val HASH_VACANT = 0.toByte
  final val HASH_REMOVED = 1.toByte
  final val HASH_USED = 2.toByte

  var items: ResizableArray[T] = new ResizableArray[T]()
  var buckets: ResizableArray[Byte] = new ResizableArray[Byte]()
  var length: Int = items.capacity
  var used: Int = 0

  def mask = buckets.length - 1
  def limit = (buckets.length * Settings.HashLoadFactor).toInt

  /**
   * Locates the index of the specific element.
   * @param x The element to be found
   * @return Index. If not found, -1.
   */
  def locate(x: T): Int = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask // initial bucket
      val status = buckets(j)
      if (status == HASH_VACANT)
        return -1
      else if (status == HASH_USED && items(j) =~= x)
        return j
      i = i * 5 + p + 1
      p >>= 5
    } while (true)
    -1
  }

  def insert(x: T): Boolean = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask
      val status = buckets(j)
      if (status == HASH_USED) {
        if (items(j) =~= x) return false
        else {
          i = i * 5 + p + 1
          p >>= 5
        }
      }
      else if (status == HASH_REMOVED & locate(x) != -1)
        return false //TODO: performance?
      else {
        items(j) = x
        buckets(j) = HASH_USED
        length += 1
        if (status == 0) {
          used += 1
          if (used > limit) items.ensureCapacity(items.length * 2)
        }
        return true
      }
    } while (true)
    false
  }
  
  def remove(x: T): Boolean = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask
      val status = buckets(j)
      if (status == HASH_USED && items(j) == x) {
        buckets(j) = HASH_REMOVED
        length -= 1
        return true
      }
      else if (status == HASH_VACANT) {
        return false
      }
      else {
        i = i * 5 + p + 1
        p >>= 5
      }
    } while (true)
    false
  }

}
