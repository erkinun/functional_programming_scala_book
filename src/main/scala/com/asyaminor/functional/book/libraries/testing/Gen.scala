package com.asyaminor.functional.book.libraries.testing

import com.asyaminor.functional.book.functional_state.State.Rand
import com.asyaminor.functional.book.functional_state.{RNG, State}
import com.asyaminor.functional.book.libraries.testing.Prop.{FailedCase, SuccessCount}

import scala.collection.immutable

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = Prop.this.check && p.check
//  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

object Gen {
  //def listOf[A](a: Gen[A]): Gen[List[A]]
  //def forAll[A](a: Gen[A])(f: A => Boolean): Prop

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
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(State.flatMap(sample)(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State.sequence(List.fill(s)(sample))))
}