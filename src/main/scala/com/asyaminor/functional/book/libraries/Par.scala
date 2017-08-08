package com.asyaminor.functional.book.libraries

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit


object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get))
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val filtered = as.map(asyncF(a => if(f(a)) List(a) else List()))
    map(sequence(filtered))(_.flatten)
  }
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val nResult = run(es)(n).get()
      choices(nResult)(es)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val choiceSelector = map(cond)(b => if(b) 1 else 0)
    val choices = List(f, t)

    choiceN(choiceSelector)(choices)
  }

  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get()
      choices(a)(es)
    }

  def choiceChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    flatMap(cond)(b => if(b) t else f)
  }

  def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    flatMap(n)(num => choices(num))
  }
}
