package poly.collection.impl

import poly.collection._
import poly.macroutil._

/**
 * @author Tongfei Chen
 */
final class ResizableTable[T]
(private[this] var capRow: Int = Settings.ArrayInitialSize,
 private[this] var capCol: Int = Settings.ArrayInitialSize) { self =>

  private[this] var data: Array[AnyRef] =
    Array.ofDim[AnyRef](math.max(
      nextHalfPowerOfTwo(capRow) * nextHalfPowerOfTwo(capCol),
      Settings.ArrayInitialSize * Settings.ArrayInitialSize
    ))

  def ensureRowCapacity(minCapRow: Int): Unit = {
    if (capRow < minCapRow) {
      val newCapRow = nextHalfPowerOfTwo(minCapRow)
      val newData = Array.ofDim[AnyRef](newCapRow * capCol)
      System.arraycopy(data, 0, newData, 0, capRow * capCol)
      data = newData
      capRow = newCapRow
    }
  }

  def ensureColCapacity(minCapCol: Int): Unit = {
    if (capCol < minCapCol) {
      val newCapCol = nextHalfPowerOfTwo(minCapCol)
      val newData = Array.ofDim[AnyRef](capRow * newCapCol)
      for (i <- 0 until capRow)
        System.arraycopy(data, i * capCol, newData, i * newCapCol, capCol)
      data = newData
      capCol = newCapCol
    }
  }

  def ensureRowColCapacity(minCapRow: Int, minCapCol: Int): Unit = {
    if (capRow >= minCapRow) ensureColCapacity(minCapCol)
    else if (capCol >= minCapCol) ensureRowCapacity(minCapRow)
    else {
      val newCapRow = nextHalfPowerOfTwo(minCapRow)
      val newCapCol = nextHalfPowerOfTwo(minCapCol)
      val newData = Array.ofDim[AnyRef](newCapCol * newCapCol)
      for (i <- 0 until capRow) {
        System.arraycopy(data, i * capCol, newData, i * newCapCol, capCol)
      }
      data = newData
      capRow = newCapRow
      capCol = newCapCol
    }
  }

  def rowCapacity = capRow
  def colCapacity = capCol

  def apply(i: Int, j: Int) = data(i * capCol + j).asInstanceOf[T]

  def get(i: Int, j: Int): AnyRef = data(i * capCol + j)

  def set(i: Int, j: Int, x: AnyRef): Unit = {
    if (i >= capRow && j >= capCol) ensureRowColCapacity(i, j)
    else if (i >= capRow) ensureRowCapacity(i)
    else if (j >= capCol) ensureColCapacity(j)
    data(i * capCol + j) = x
  }

  def update(i: Int, j: Int, x: T) = set(i, j, x.asInstanceOf[AnyRef])

  def fillInplace(x: T) = FastLoop.ascending(0, capRow * capCol, 1) { i =>
    data(i) = x.asInstanceOf[AnyRef]
  }

  def fillWithNull() = FastLoop.ascending(0, capRow * capCol, 1) { i =>
    data(i) = null
  }

}
