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

  def foldRightWithLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((x, y) => f(y, x))
  def sum4(as: List[Int]): Int = foldRightWithLeft(as, 0)((x, y) => x + y)

  //def add(sum: Int, elem: Int): Int = sum + elem
  def sum3(as: List[Int]): Int = foldLeft(as, 0)((x,y) => x + y)
  def product3(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  def length3(as: List[Int]): Int = foldLeft(as, 0)((x, _) => x + 1)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,y) => y + 1)

  def reverse[A](as: List[A]): List[A] = foldRight(as, Nil:List[A])((x, y) => Cons(x, y))

  // Implement append in terms of either foldLeft or foldRight.
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((elem, nextList) => Cons(elem, nextList))

  def concat[A](xss: List[List[A]]): List[A] = foldLeft(xss, Nil:List[A])((cur, list) => append(cur, list))

  def add1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def add2(xs: List[Int]): List[Int] = foldRight(xs, Nil:List[Int])((item, cur) => Cons(item + 1, cur))

  def toString(ds: List[Double]): String = ds match {
    case Nil => ""
    case Cons(h, t) => h.toString + toString(t)
  }

  def toStr2(ds: List[Double]): String = foldLeft(ds, "")((acc, d) => acc + d.toString)

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    def inner(as: List[A], f: A => B, acc: List[B]): List[B] = as match {
      case Nil => acc
      case Cons(h, t) => inner(t, f, Cons(f(h), acc))
    }
    inner(as, f, Nil:List[B])
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def inner(as: List[A], f: A => Boolean, acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) =>
        if (f(h)) inner(t, f, Cons(h, acc))
        else inner(t, f, acc)
    }

    inner(as, f, Nil:List[A])
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {

    def inner(as: List[A], f: A => List[B], acc: List[B]): List[B] = as match {
      case Nil => acc
      case Cons(h, t) => inner(t, f, append(acc, f(h)))
    }

    inner(as, f, Nil: List[B])
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
