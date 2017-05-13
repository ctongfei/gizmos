package poly.collection.correct

import cats.implicits._
import org.scalacheck._
import org.scalacheck.Prop._
import poly.{collection => pc}

import scala.{collection => sc}

import poly.collection.conversion.FromScala._
import poly.collection.testutil.TestUtil._

/**
 * @author Tongfei Chen
 */
object IterableTest extends Properties("Iterable") {

  case class TestCase(val p: pc.Iterable[Int], val s: sc.Iterable[Int])

  case class TestCase2(val p1: pc.Iterable[Int], val p2: pc.Iterable[Int], val s1: sc.Iterable[Int], val s2: Iterable[Int])

  case class TestCaseI(val p: pc.Iterable[Int], val s: sc.Iterable[Int], n: Int)

  implicit class Ops[T](val s: scala.collection.Iterable[T]) {
    def ~~(p: poly.collection.Iterable[T]) = checkIterable(s, p)
  }

  implicit val g1 = Arbitrary {
    for {
      s <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield TestCase(s.asPoly.asIterable, s)
  }

  implicit val g2 = Arbitrary {
    for {
      s1 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      s2 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
    } yield TestCase2(s1.asPoly.asIterable, s2.asPoly.asIterable, s1, s2)
  }

  implicit val gI = Arbitrary {
    for {
      s <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      n <- Arbitrary.arbInt.arbitrary
    } yield TestCaseI(s.asPoly.asIterable, s, n)
  }

  property("id")      = forAll { c: TestCase =>
    c.s ~~ c.p
  }

  property("map")     = forAll { c: TestCase =>
    c.s.map(_ + 1) ~~ c.p.map(_ + 1)
  }

  property("filter")  = forAll { c: TestCase =>
    c.s.filter(_ < 10) ~~ c.p.filter(_ < 10)
  }

  property("filterNot") = forAll { c: TestCase =>
    c.s.filterNot(_ < 10) ~~ c.p.filterNot(_ < 10)
  }

  property("flatMap") = forAll { c: TestCase =>
    c.s.flatMap(x => Iterable.fill(x % 10)(x)) ~~ c.p.flatMap((x: Int) => pc.Iterable.repeat(x % 10)(x))
  }

  property("product") = forAll { c: TestCase =>
    (for (x <- c.s; y <- c.s) yield (x, y)) ~~ (c.p product c.p)
  }

  property("productWith") = forAll { c: TestCase =>
    (for (x <- c.s; y <- c.s) yield x + y) ~~ (c.p productWith c.p)(_ + _)
  }

  property("zip") = forAll { c: TestCase2 =>
    (c.s1 zip c.s2) ~~ (c.p1 zip c.p2)
  }

  property("zipWith") = forAll { c: TestCase2 =>
    (c.s1 zip c.s2).map { case (x, y) => x + y } ~~ (c.p1 zipWith c.p2)(_ + _)
  }

  property("collect") = forAll { c: TestCase =>
    c.s.collect { case x if x < 10 => x } ~~ c.p.collect { case x if x < 10 => x }
  }

  property("collectOption") = forAll { c: TestCase =>
    c.s.flatMap { x => if (x < 10) Some(x) else None } ~~ c.p.collectOption { x => if (x < 10) Some(x) else None }
  }

  property("distinct") = forAll { c: TestCase =>
    c.s.toSeq.distinct ~~ c.p.distinct
  }

  // distinctBy

  property("union") = forAll { c: TestCase2 =>
    (c.s1 ++ c.s2).toSeq.distinct ~~ (c.p1 union c.p2)
  }

  // intersect

  property("concat") = forAll { c: TestCase2 =>
    (c.s1 ++ c.s2) ~~ (c.p1 concat c.p2)
  }

  property("prepend") = forAll { c: TestCaseI =>
    (Iterable(c.n) ++ c.s) ~~ (c.p prepend c.n)
  }

  property("append") = forAll { c: TestCaseI =>
    (c.s ++ Iterable(c.n)) ~~ (c.p append c.n)
  }

  property("tail") = forAll { c: TestCase =>
    (Iterable(0) ++ c.s).tail ~~ (c.p prepend 0).tail
  }

  property("init") = forAll { c: TestCase =>
    (c.s ++ Iterable(0)).init ~~ (c.p append 0).init
  }

  property("take") = forAll { c: TestCaseI =>
    (c.s take c.n) ~~ (c.p take c.n)
  }

  property("takeWhile") = forAll { c: TestCase =>
    (c.s takeWhile { _ > 0 }) ~~ (c.p takeWhile { _ > 0 })
  }

  // takeTo

  property("takeUntil") = forAll { c: TestCase =>
    (c.s takeWhile { _ < 0 }) ~~ (c.p takeUntil { _ >= 0 })
  }

  property("drop") = forAll { c: TestCaseI =>
    (c.s drop c.n) ~~ (c.p drop c.n)
  }

  property("dropWhile") = forAll { c: TestCase =>
    (c.s dropWhile {_ < 0}) ~~ (c.p dropWhile {_ < 0})
  }

  property("dropTo") = forAll { c: TestCase =>
    val s = (c.s ++ Iterable(-1)) dropWhile { _ >= 0 }
    s.tail ~~ (c.p append -1 dropTo { _ < 0 })
  }

  property("dropUntil") = forAll { c: TestCase =>
    (c.s dropWhile { _ < 0 }) ~~ (c.p dropUntil { _ >= 0 })
  }

  property("withIndex") = forAll { c: TestCase =>
    (c.s.zipWithIndex.map(_.swap) ~~ c.p.withIndex)
  }

  // repeat
  // cycle
  // interleave

  property("scanLeft") = forAll { c: TestCase =>
    c.s.scanLeft(0)(_+_) ~~ c.p.scanLeft(0)(_+_)
  }

  property("slidingPairs") = forAll { c: TestCase =>
    c.s.iterator.sliding(2).withPartial(false).map(x => (x.head, x.last)).toIterable ~~ c.p.slidingPairs
  }



  property("repeat")  = forAll { c: Int =>
    sc.Iterable.fill(c % 100)(c) ~~ pc.Iterable.repeat(c % 100)(c)
  }
}
