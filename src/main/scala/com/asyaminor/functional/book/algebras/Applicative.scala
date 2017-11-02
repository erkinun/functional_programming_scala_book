package com.asyaminor.functional.book.algebras

import com.asyaminor.functional.book.datastructures.{Failure, Success, Validation}

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

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldLeft(unit(Map():Map[K,V]))((acc, item) => {
    map2(acc, item._2)((map, value) => {
      val key = item._1
      if (!map.contains(key)) map + (key -> value)
      else map
    })
  })

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }


  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = unit(a)
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = map(fa)(f)
    }
  }
}

//trait Traverse[F[_]] {
//  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
//    sequence(map(fa)(f))
//  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
//    traverse(fga)(ga => ga)
//}
//
//case class Tree[+A](head: A, tail: List[Tree[A]])
//
//object Traverse {
//  val listTraverse = new Traverse[List] {
//    def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
//      fa.foldLeft(G.unit(List[B]()))((list, a) => G.map2(f(a), list)(_ :: _))
//  }
//
//  val optionTraverse = new Traverse[Option] {
//    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
//      oa match {
//        case None => G.unit(None)
//        case Some(a) => G.map(f(a))(Some(_))
//      }
//  }
//
//  val treeTraverse = new Traverse[Tree] {
//    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
//      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
//  }
//}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A) = Stream.continually(a)
    override def map2[A,B,C](a: Stream[A], b: Stream[B])( f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = {
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A) = Success(a)
      override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a,b))
          case (first@Failure(_, _), Success(_)) => first
          case (Success(_), second@Failure(_, _)) => second
          case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ Vector(hb) ++ tb)
        }
    }
  }
}
