package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiSeq[+T] extends Seq[T] {

  def headNode: BiSeqNode[T]
  def lastNode: BiSeqNode[T]



}
