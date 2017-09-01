package com.asyaminor.functional.book.libraries.testing

import com.asyaminor.functional.book.functional_state.State.Rand
import com.asyaminor.functional.book.laziness.Stream
import com.asyaminor.functional.book.functional_state.{RNG, State}
import com.asyaminor.functional.book.libraries.testing.Prop.{FailedCase, SuccessCount, TestCases}


case class Prop(run: (TestCases, RNG) => Result)

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

//  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//    (n,rng) => randomStream(as)(rng).zipAll(Stream.from(0)).take(n).map {
//      case (a, i) => try {
//        if (f(a)) Passed else Falsified(a.toString, i)
//      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
//    }.find(_.isFalsified).getOrElse(Passed)
//  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rand: Rand[Int] = State.nonNegativeLessThan(stopExclusive - start)
    Gen(State.map(rand)(n => n + start))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = {
    val someGen: Gen[Int] = choose(0, 2)
    Gen(State.map(someGen.sample)(i => if (i == 0) true else false))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val hede: List[State[RNG, A]] = List.fill(n)(g.sample)
    Gen(State.sequence(hede))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(res => if(res) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val w1 = g1._2
    val w2 = g2._2

    val ratio = w1.min(w2) / (w1 + w2)
    val rangeGen = Gen.choose(0, 100)

    rangeGen.flatMap(r => if(r < 100 * ratio) g1._1 else g2._1)
  }
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(State.flatMap(sample)(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State.sequence(List.fill(s)(sample))))
}