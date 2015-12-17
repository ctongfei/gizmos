package poly.collection.mut

import poly.collection._
import scala.reflect._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
object Auto {

  def Map[K: ClassTag, V]: Map[K, V] = implicitly[ClassTag[K]].runtimeClass match {
    case Integer.TYPE => ???
    case _ => ???
  }

  def Graph[K: ClassTag, V, E]: Graph[K, V, E] = implicitly[ClassTag[K]].runtimeClass match {
    case Integer.TYPE => ???
    case _ => ???
  }

}
