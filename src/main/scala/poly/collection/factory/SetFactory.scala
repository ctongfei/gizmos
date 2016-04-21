package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
  * Generic factory for companion objects of set types.
 *
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait SetFactory[S[_]] extends FactoryWithEquiv[S]

trait SortedSetFactory[S[_]] extends FactoryWithOrder[S]

trait HashSetFactory[S[_]] extends FactoryWithIntHashing[S]
