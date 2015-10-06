package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ops {

  implicit class withCollectionOps[T](val x: T) extends AnyVal {

    def single = IndexedSeq.fill(1)(x)

    def infinite = Seq.infinite(x)

    def repeat(n: Int) = IndexedSeq.fill(n)(x)

    def iterate(f: T => T) = Seq.iterate(x)(f)

  }

}
