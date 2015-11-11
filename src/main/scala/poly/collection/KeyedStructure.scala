package poly.collection

import poly.algebra.specgroup._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait KeyedStructure[@sp(i) K, +Repr <: KeyedStructure[K, Repr]] extends Keyed[K] {

  def filterKeys(f: K => Boolean): Repr

}
