package poly.collection.impl

import poly.collection._

/**
 * @author Tongfei Chen
 */
class BitArray(val data: Array[Byte], val fastLength: Int, val offset: Int = 0) extends AbstractIndexedSeq[Boolean] {

  import BitArray._

  def fastApply(bitIdx: Int) = {
    val byteIdx = (bitIdx + offset) / ByteSize
    val bit = (bitIdx + offset) % ByteSize
    (data(byteIdx) & (1 << bit)) != 0
  }

  def update(bitIdx: Int, v: Boolean) = {
    val byteIdx = (bitIdx + offset) / ByteSize
    val bit = (bitIdx + offset) % ByteSize
    if (v)
      // data(byteIdx) |= (1 << bit) TODO: SI-8718
      data(byteIdx) = (data(byteIdx) | (1 << bit)).toByte
    else
      // data(byteIdx) &= ~(1 << bit)
      data(byteIdx) = (data(byteIdx) & ~(1 << bit)).toByte
  }

  override def take(n: Int) = new BitArray(this.data, n, this.offset)

  override def drop(n: Int) = new BitArray(this.data, this.length - n, this.offset + n)

  override def slice(i: Int, j: Int) = this drop i take (j - i)

  override def newIterator: Iterator[Boolean] = new AbstractIterator[Boolean] {
    private[this] var byteIdx = (offset - 1) / ByteSize
    private[this] var bit = (offset - 1) %+ ByteSize
    private[this] var i = -1

    def current: Boolean = (data(byteIdx) & (1 << bit)) != 0
    def advance(): Boolean = {
      if (bit == ByteSize - 1) {
        byteIdx += 1
        bit = 0
      } else bit += 1
      i += 1
      i < size
    }
  }

  override def equals(obj: Any) = obj match {
    case that: BitArray => LexicographicOrder.compare(this, that) == 0
    case _ => false
  }

  override def toString = map(b => if (b) "1" else "0").buildString("")

}

object BitArray {

  final val ByteSize = 8

  implicit object LexicographicOrder extends Order[BitArray] {
    val booleanOrder = cats.instances.boolean.catsKernelStdOrderForBoolean
    def compare(x: BitArray, y: BitArray): Int = { // manual implementation for zip: no boxing
      var xi = x.newIterator
      var yi = y.newIterator
      while (xi.advance() && yi.advance()) {
        val xc = xi.current
        val yc = yi.current
        if (!xc && yc) return -1
        if (xc && !yc) return 1
      }
      booleanOrder.compare(xi.advance(), yi.advance())
    }
  }

}