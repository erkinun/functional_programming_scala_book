package com.asyaminor.functional.book.libraries.testing

trait Prop { def &&(p: Prop): Prop }

trait Gen[T] {
  def listOf[A](a: Gen[A]): Gen[List[A]]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
