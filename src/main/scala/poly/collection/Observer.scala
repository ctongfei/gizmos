package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Observer[-T] { self =>

  def current: Nothing = ???
  def current_=(x: T): Unit

  def advance(hasElem: Boolean): Unit

  def write(x: Option[T]) = x match {
    case Some(elem) => {
      self.current = elem
      advance(true)
    }
    case None => advance(true)
  }

}
