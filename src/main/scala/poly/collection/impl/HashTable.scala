package poly.collection.impl

import poly.algebra._
import poly.algebra.ops._
import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class HashTable[K, V](implicit val hashing: IntHashing[K]) {

  import HashTable._

  val keys: ResizableSeq[K] = new ResizableSeq[K]()
  val vals: ResizableSeq[V] = new ResizableSeq[V]()
  val stat: ResizableSeq[EntryStatus] = new ResizableSeq[EntryStatus]()
  var size: Int = 0
  var used: Int = 0

  def mask = stat.fastLength - 1
  def limit = (stat.fastLength * Settings.HashTableLoadFactor).toInt

  protected def grow() = {
    keys.grow()
    vals.grow()
    stat.grow()
  }

  /**
   * Locates the index of the specific element.
   * @param x The element to be found
   * @return Index. If not found, -1.
   */
  def locate(x: K): Int = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask // initial bucket
      val status = stat(j)
      if (status == vacant)
        return -1
      else if (status == inUse && keys(j) =~= x)
        return j
      i = i * 5 + p + 1
      p >>= 5
    } while (true)
    -1
  }

  def insert(x: K): Boolean = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask
      val status = stat(j)
      if (status == inUse) {
        if (keys(j) =~= x) return false
        else {
          i = i * 5 + p + 1
          p >>= 5
        }
      }
      else if (status == removed & locate(x) != -1)
        return false //TODO: performance?
      else {
        keys(j) = x
        stat(j) = inUse
        size += 1
        if (status == vacant) {
          used += 1
          if (used > limit) grow()
        }
        return true
      }
    } while (true)
    false
  }
  
  def remove(x: K): Boolean = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = i & mask
      val status = stat(j)
      if (status == inUse && keys(j) =~= x) {
        stat(j) = removed
        size -= 1
        return true
      }
      else if (status == vacant) {
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

object HashTable {
  private[poly] type EntryStatus = Byte
  private[poly] final val vacant = 0.asInstanceOf[Byte]
  private[poly] final val removed = 1.asInstanceOf[Byte]
  private[poly] final val inUse = 2.asInstanceOf[Byte]
}
