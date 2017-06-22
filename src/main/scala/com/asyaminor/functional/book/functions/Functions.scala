package com.asyaminor.functional.book.functions

object Functions {
  def juxta[A](fs: List[(A) => A])(a: A): List[A] = fs match {
    case Nil => Nil
    case h :: t => h(a) :: juxta(t)(a)
  }

  val sq = (num: Int) => num * num
  val add = (a:Int, b:Int) => a + b
  val plus5 = (num: Int) => num + 5
}
