package poly.collection

/**
 * @author Tongfei Chen
 */
object specgroup {

  type unsp = scala.annotation.unspecialized
  type sp = scala.specialized
  final val fd = new Specializable.Group((Float, Double))
  final val di = new Specializable.Group((Int, Double))
  final val fdi = new Specializable.Group((Int, Float, Double))
  final val dil = new Specializable.Group((Int, Double, Long))
  final val fdil = new Specializable.Group((Int, Float, Double, Long))
  final val fdib = new Specializable.Group((Int, Float, Double, Boolean))
  final val fdilb = new Specializable.Group((Int, Float, Double, Boolean, Long))
  final val dib = new Specializable.Group((Int, Double, Boolean))
  final val ib = new Specializable.Group((Int, Boolean))
  final val il = new Specializable.Group((Int, Long))
  final val ilb = new Specializable.Group((Int, Long, Boolean))

  /**
   * Specializes what [[scala.Tuple1]] specialized.
   */
  final val spTuple1 = new Specializable.Group((Int, Long, Double))

  /**
   * Specializes what [[scala.Tuple2]] specialized.
   */
  final val spTuple2 = new Specializable.Group((Int, Long, Double, Char, Boolean))

  /**
   * Specializes what the argument of [[scala.Function1]] specialized.
   */
  final val spFunc1 = new Specializable.Group((Int, Long, Float, Double))

  /**
   * Specializes what the result type of [[scala.Function1]]/[[scala.Function2]] specialized.
   */
  final val spFuncR = new Specializable.Group((Unit, Boolean, Int, Float, Long, Double))

  /**
   * Specializes what the argument of [[scala.Function2]] specialized.
   */
  final val spFunc2 = new Specializable.Group((Int, Long, Double))
}
