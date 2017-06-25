package com.asyaminor.functional.book.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionFr: Option[A] = foldRight(None:Option[A])((a, b) => b match {
    case None => Some(a)
    case Some(_) => Some(a)
  })

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) Empty
    else Cons(h, () => t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => if (n == 0) this
    else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(p(h())) Cons(h, () => t().takeWhile(p))
    else t().takeWhile(p)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAllFr(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight(Empty:Stream[B])((a, b) => Stream.cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty:Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  def append[B >: A](elem: => Stream[B]): Stream[B] = foldRight(elem)((a,b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append(b))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}