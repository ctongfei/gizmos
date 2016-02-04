package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait PairIterator[+A, +B] extends Iterator[(A, B)] {

  def currentFirst: A

  def currentSecond: B

  def current = (currentFirst, currentSecond)

}
