package com.asyaminor.functional.book.algebras

trait Applicative[F[_]] extends Functor[F] {

  // define in terms of map2 and unit
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((a2b, a) => a2b(a))
  def unit[A](a: => A): F[A]

  // define in terms of apply
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val curried: A => B => C = f.curried
    val fbc: F[B => C] = apply(unit(curried))(fa)
    apply(fbc)(fb)
  }

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curried: A => B => C => D = f.curried

    apply(apply(apply(unit(curried))(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))((a, b) => a :: b)
  }
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))
}
