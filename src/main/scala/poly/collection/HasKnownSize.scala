package poly.collection

/**
 * Marker trait for a collection whose size can be efficiently retrieved.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait HasKnownSize {

  def size: Int

}
