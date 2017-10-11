package com.asyaminor.functional.book.algebras

import java.time.MonthDay

import com.asyaminor.functional.book.datastructures
import com.asyaminor.functional.book.functional_state.State
import com.asyaminor.functional.book.libraries.testing.Gen

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] {

  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    val firstFunction: Unit => F[A] = _ => ma
    val result: (Unit) => F[B] = compose(firstFunction, f)
    result()
  }

  def flatMapWithJoin[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
  def composeWithJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => flatMap(ma)(a => unit(a)))

  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))


  def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
    case Nil => unit(Nil)
    case h :: t => flatMap(h)(a => map(sequence(t))(listA => a :: listA))
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b => map(traverse(t)(f))(list => b :: list))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b => if(b) map(filterM(t)(f))(h :: _) else filterM(t)(f))
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => {
    flatMap(f(a))(b => g(b))
  }
}

object Monad {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A) = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]) = ma.flatMap(a => f(a))
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A) = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) = ma.flatMap(a => f(a))
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A) = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]) = ma.flatMap(a => f(a))
  }

//  val stateMonad = new Monad[State] {
//    override def unit[A](a: => A) = State.unit(a)
//    override def flatMap[A, B](ma: State[A])(f: (A) => State[B]) = ???
//  }

}
