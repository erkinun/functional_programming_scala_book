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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //def add(sum: Int, elem: Int): Int = sum + elem
  def sum3(as: List[Int]): Int = foldLeft(as, 0)((x,y) => x + y)
  def product3(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  def length3(as: List[Int]): Int = foldLeft(as, 0)((x, _) => x + 1)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,y) => y + 1)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
