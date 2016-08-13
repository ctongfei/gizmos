package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._

/**
 * @author Tongfei Chen
 */
object AutoMultimap extends BuilderFactoryAB_EvAB[KeyMutableMultimap, Eq, Eq] {

  implicit def newBuilder[A: Eq, B: Eq] = new Builder[(A, B), KeyMutableMultimap[A, B]] {

    private[this] val m = AutoMap[A, KeyMutableSet[B]]()

    def addInplace(x: (A, B)) = {
      val (a, b) = x
      if (m containsKey a) m(a) += b
      else m += a -> AutoSet(b)
    }

    def result = new MapOfSetsMultimap[A, B](m)
}

}
