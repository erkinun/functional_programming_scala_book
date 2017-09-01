package com.asyaminor.functional.book.libraries.parser

object Parsing {

}

trait Parsers[ParseError, Parser[+_]] {

  self =>

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  //def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(aVal => succeed(f(aVal)))

  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p.flatMap(a => p2.flatMap(b => succeed((a, b))))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)


  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = p.flatMap(a => p2.map(b => f(a,b)))
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))((a, b) => a :: b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = map2(succeed(n), p){ case (a, b) => Range(0, a).map(_ => b).toList }

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    val numA: Parser[Int] = char('a').many.map(_.size)
    val hede = char('a') ** char('b')
  }
}