package com.asyaminor.functional.book

import scala.annotation.tailrec

object Application  {
  def main(args: Array[String]): Unit = {
    println("hello world")

    println(fib(6))
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
}
