package poly.collection.mut

import poly.collection._
import scala.reflect._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
object Auto {

  private val PrimitiveIntType = java.lang.Integer.TYPE

  def Map[K: ClassTag, V]: Map[K, V] = implicitly[ClassTag[K]].runtimeClass match {
    case PrimitiveIntType => ???
    case _ => ???
  }

  def Graph[K: ClassTag, V, E]: Graph[K, V, E] = implicitly[ClassTag[K]].runtimeClass match {
    case PrimitiveIntType => ???
    case _ => ???
  }

}
