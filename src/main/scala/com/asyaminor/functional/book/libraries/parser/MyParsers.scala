package com.asyaminor.functional.book.libraries.parser

import com.asyaminor.functional.book.libraries.parser.fpinscala.parsing.{Location, ParseError, Parsers}

import scala.util.matching.Regex

class MyParsers[+A] {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

}

object MyParsers extends Parsers[MyParsers] {
  override def run[A](p: MyParsers[A])(input: String): Either[ParseError, A] = ???

  override implicit def string(s: String): MyParsers[String] = ???

  override def succeed[A](a: A): MyParsers[A] = ???

  override def slice[A](p: MyParsers[A]): MyParsers[String] = ???

  override def or[A](p1: MyParsers[A], p2: => MyParsers[A]): MyParsers[A] = ???

  override def flatMap[A, B](p: MyParsers[A])(f: (A) => MyParsers[B]): MyParsers[B] = ???

  override implicit def regex(r: Regex): MyParsers[String] = ???

  override def label[A](msg: String)(p: MyParsers[A]): MyParsers[A] = ???

  override def scope[A](msg: String)(p: MyParsers[A]): MyParsers[A] = ???

  override def attempt[A](p: MyParsers[A]): MyParsers[A] = ???
}
