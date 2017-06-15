package com.asyaminor.functional.book.datastructures


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}
case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = Some(f(get))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(f: (A) => Boolean): Option[A] = if (f(get)) this else None
}
case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = this
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    Some(xs).flatMap(xs => mean(xs))
      .map(m => xs.map(x => math.pow(x - m, 2)))
      .flatMap(xs => mean(xs))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => None
  }

  def map3[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av,bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h.flatMap(a => sequence(t) map(listA => Cons(a, listA)))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => f(h) flatMap(b => traverse(t)(f) map(list => Cons(b, list)))
  }
}