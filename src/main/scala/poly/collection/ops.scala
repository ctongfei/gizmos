package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
object ops {

  implicit class withCollectionOps[T](val x: T) extends AnyVal {

    /**
     * Constructs a sequence of length 1 with this specific element.
     * @return A sequence with only `this` element
     * @example {{{3.single == (3)}}}
     */
    def single = IndexedSeq.fill(1)(x)

    /**
     * Constructs a lazy sequence that repeats the specific element infinitely.
     * @return An infinite sequence
     * @example {{{2.cycle == (2, 2, 2, 2, 2, ...)}}}
     */
    def cycle = Iterable.infinite(x)

    /**
     * Constructs a lazy sequence that repeats the specific element for the given number of times.
     * @example {{{4.repeat(5) == (4, 4, 4, 4, 4)}}}
     */
    def repeat(n: Int) = IndexedSeq.fill(n)(x)

    /**
     * Constructs a lazy infinite sequence by iteratively applying a function from a starting element.
     * @example {{{0.iterate(_ + 2) == (0, 2, 4, 6, 8, ...)}}}
     * @return
     */
    def iterate(f: T => T) = Seq.iterate(x)(f)

  }

}
