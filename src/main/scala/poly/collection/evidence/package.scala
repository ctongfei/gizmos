package poly.collection

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
package object evidence {

   //type &[E1[_], E2[_]] = ({type λ[T] = Ev2[E1, E2, T]})#λ

  /**
   * A type lambda that has kind (* => *) and an implicit variable of such type
   * can always be found (delegated to [[DummyImplicit]]).
   */
  type NoneEv[T] = DummyImplicit

}
