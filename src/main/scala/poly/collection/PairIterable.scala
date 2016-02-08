package poly.collection

import poly.algebra.hkt._


/**
 * @author Tongfei Chen
 */
trait PairIterable[+A, +B] extends Iterable[(A, B)] { self =>

  import Iterable._
  import PairIterable._

  def newIterator: PairIterator[A, B]

  def foreach2[V](f: (A, B) => V) = {
    val i = newIterator
    while (i.advance()) f(i.currentFirst, i.currentSecond)
  }

  def map2[C](f: (A, B) => C) = ofIterator {
    new Iterator[C] {
      private[this] val i = self.newIterator
      def current = f(i.currentFirst, i.currentSecond)
      def advance() = i.advance()
    }
  }

  def filter2(f: (A, B) => Boolean) = ofPairIterator {
    new PairIterator[A, B] {
      private[this] val i = self.newIterator
      def currentFirst = i.currentFirst
      def currentSecond = i.currentSecond
      def advance(): Boolean = {
        do {
          val hasNext = i.advance()
          if (!hasNext) return false
        } while (!f(i.currentFirst, i.currentSecond))
        false
      }
    }
  }

  def filterNot2(f: (A, B) => Boolean) = filter2((a, b) => !f(a, b))

  def concat2[C >: A, D >: B](that: PairIterable[C, D]): PairIterable[C, D] = ofPairIterator {
    new PairIterator[C, D] {
      private[this] var i: PairIterator[C, D] = self.newIterator
      private[this] var first = true
      def currentFirst = i.currentFirst
      def currentSecond = i.currentSecond
      def advance(): Boolean = {
        if (i.advance()) true
        else if (first) {
          i = that.newIterator
          first = false
          i.advance()
        }
        else false
      }
    }
  }

  override def tail: PairIterable[A, B] = {
    val tailIterator = self.newIterator
    tailIterator.advance()
    ofPairIterator(tailIterator)
  }




}

object PairIterable {

  
  def ofPairIterator[A, B](i: => PairIterator[A, B]): PairIterable[A, B] = new AbstractPairIterable[A, B] {
    def newIterator = i // call-by-name parameter!
  }

  
  // TYPECLASS INSTANCES
  object Bifunctor extends Bifunctor[PairIterable] {
    def mapFirst[A, B, C](x: PairIterable[A, B])(f: A => C) = ??? // x.map2 { (a, b) => (f(a), b) }
    def mapSecond[A, B, D](x: PairIterable[A, B])(f: B => D) = ??? // x.map2 { ()}
  }
  
}

abstract class AbstractPairIterable[+A, +B] extends AbstractIterable[(A, B)] with PairIterable[A, B]
