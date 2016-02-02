package poly.collection.impl

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.impl.ClosedHashingSet._

/**
 * @author Tongfei Chen
 */
class ClosedHashingSet[K: IntHashing] (
  private[collection] var capacity: Int = Settings.ArrayInitialSize
) {
  scala.collection.mutable.HashMap
  private[collection] var keys: Array[AnyRef] = Array.ofDim[AnyRef](capacity)
  private[collection] var stat: Array[EntryStatus] = Array.ofDim[EntryStatus](capacity)
  private[collection] var size: Int = 0
  private[collection] var used: Int = 0

  private[collection] var mask = capacity - 1
  private[collection] var limit = (capacity * Settings.HashTableLoadFactor).toInt

  @inline private def initialSlot(i: Int) = {
    val r = i % mask
    if (r < 0) r + mask else r
  }

  protected def grow() = {
    val newCapacity = nextPowerOfTwo(capacity + 1)
    val newSet = new ClosedHashingSet[K](newCapacity)
    for (i ← Range(capacity)) {
      if (stat(i) == INUSE)
        newSet.insert(keys(i).asInstanceOf[K])
    }
    this.keys = newSet.keys
    this.stat = newSet.stat
    this.size = newSet.size
    this.used = newSet.used
    this.mask = newSet.mask
    this.limit = newSet.limit
    this.capacity = newSet.capacity
  }

  /**
   * Locates the index of the specific element.
 *
   * @param x The element to be found
   * @return Index. If not found, -1.
   */
  def locate(x: K): Int = {
    var i = x.### & 0x7fffffff
    var p = i
    do {
      val j = initialSlot(i) // initial bucket
      val status = stat(j)
      if (status == VACANT)
        return -1
      else if (status == INUSE && x === keys(j).asInstanceOf[K])
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
      val j = initialSlot(i)
      val status = stat(j)
      if (status == INUSE) {
        if (x === keys(j).asInstanceOf[K]) return false
        else {
          i = i * 5 + p + 1
          p >>= 5
        }
      }
      else if (status == REMOVED & locate(x) != -1) // already there
        return false //TODO: performance?
      else {
        keys(j) = x.asInstanceOf[AnyRef]
        stat(j) = INUSE
        size += 1
        if (status == VACANT) {
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
      val j = initialSlot(i)
      val status = stat(j)
      if (status == INUSE && x === keys(j).asInstanceOf[K]) {
        stat(j) = REMOVED
        size -= 1
        return true
      }
      else if (status == VACANT) {
        return false
      }
      else {
        i = i * 5 + p + 1
        p >>= 5
      }
    } while (true)
    false
  }

  override def toString = {
    val sb = new StringBuilder()
    for (i ← Range(capacity))
      if (stat(i) == INUSE)
        sb.append(s"${keys(i)} ")
    sb.result()
  }

}

object ClosedHashingSet {
  private[collection] type EntryStatus = Byte
  private[collection] final val VACANT = 0.asInstanceOf[Byte]
  private[collection] final val REMOVED = 1.asInstanceOf[Byte]
  private[collection] final val INUSE = 2.asInstanceOf[Byte]
}
