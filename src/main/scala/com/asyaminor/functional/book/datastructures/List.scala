package com.asyaminor.functional.book.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def drop[T](l: List[T], n: Int): List[T] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[T](xs: List[T], f: T => Boolean): List[T] = xs match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f)
    else xs
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def setHead[T](xs: List[T], elem: T): List[T] = xs match {
    case Nil => Cons(elem, Nil)
    case Cons(_, tail) => Cons(elem, tail)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
