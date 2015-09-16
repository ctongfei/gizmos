package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {

  implicit class withCollectionOps[T](val x: T) extends AnyVal {

    def infinite = Iterable.infinite(x)

    def repeat(n: Int) = IndexedSeq.fill(n)(x)

    def iterate(f: T => T) = Iterable.iterate(x)(f)

  }

}
