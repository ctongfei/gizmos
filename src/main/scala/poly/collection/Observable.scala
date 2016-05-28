package poly.collection

/**
 * Represents a push-based event stream.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Observable[+T] extends Traversable[T] {

  def subscribe(o: Observer[T])

  def foreach[V](f: T ⇒ V) = subscribe {
    new Observer[T] {
      def onError(error: Throwable) = {}
      def onCompleted() = {}
      def onNext(x: T) = f(x)
    }
  }

}
