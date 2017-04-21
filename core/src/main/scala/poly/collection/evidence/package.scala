package poly.collection

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
package object evidence {

  /**
   * This trait bundles two implicit evidences on one type into one evidence.
   */
   type &[E1[_], E2[_]] = {
     type λ[T] = Ev2[E1, E2, T]
   }

  /**
   * A type lambda that has kind (* => *) of which an implicit variable
   * can always be found (delegated to [[DummyImplicit]]).
   */
  type NoneEv[T] = DummyImplicit

  /** Expresses a type constraint that witnesses a type is `Int`. */
  type IsInt[A] = A =:= Int

  type IsIntPair[A] = A =:= (Int, Int)

  private[poly] val singletonIsIntEv = =:=.tpEquals[Any]
  /** Forcibly prove that type K is `Int`. Use with caution. */
  private[poly] def evInt[K] = singletonIsIntEv.asInstanceOf[K =:= Int]


}
