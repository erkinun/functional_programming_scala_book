package com.asyaminor.functional.book

import scala.annotation.tailrec

object Application  {
  def main(args: Array[String]): Unit = {
    println("hello world")

    println(isSorted[Int](Array(1,2,3,4), ordered = (a: Int, b: Int) => a < b))
    println(isSorted[Int](Array(1,3,2,4), ordered = (a: Int, b: Int) => a < b))
    println(isSorted[Int](Array(4,3,2,1), ordered = (a: Int, b: Int) => a > b))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, sum: Int): Int = {
      if (n == 0) sum
      else go(n - 1, n * sum)
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, n1: Int, n2: Int): (Int, Int) = {
      if (n == 0) (n1, n2)
      else go(n - 1, n2, n1 + n2)
    }
    val (i1, i2) = go(n - 3, 0, 1)

    i1 + i2
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => partial1(a, f)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
