package com.asyaminor.functional.book.io

sealed trait Process[I,O] {
  def apply(s: Stream[I]): Stream[O] =
    this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h,t) => h #:: t(s)
    }

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def |>[O2](p2: Process[O,O2]): Process[I,O2] = p2 match {
    case Halt() => Halt()
    case Emit(h2, t2) => Emit(h2, |>(t2))
    case Await(rec2) => this match {
      case Emit(h, t) => t |> rec2(Some(h))
      case Halt() => Halt() |> rec2(None)
      case Await(rec1) => Await(i => rec1(i) |> p2)
    }
  }
  
}

case class Emit[I,O](
                      head: O,
                      tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]
case class Halt[I,O]() extends Process[I,O]

object Process {
  def emit[I,O](head: O,
                tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Emit(head, tail)

  def await[I,O](f: I => Process[I,O],
                 fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  def filter[I,O](p: I => Boolean): Process[I,I] = Await[I,I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double,Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None => Halt()
      }
    go(0.0)
  }

  def take[I](n: Int): Process[I,I] =
    if (n <= 0) Halt()
    else await(i => emit(i, take[I](n-1)))

  def drop[I](n: Int): Process[I,I] =
    if (n <= 0) id
    else await(_ => drop(n - 1))

  def takeWhile[I](f: I => Boolean): Process[I,I] = Await[I, I] {
    case Some(i) if f(i) => Emit(i, takeWhile(f))
    case _ => Halt()
  }

  def dropWhile[I](f: I => Boolean): Process[I,I] = await(i =>
    if (!f(i)) id
    else await(_ => dropWhile(f))
  )

  def id[I]: Process[I,I] = lift(identity)

  def count[I]: Process[I,Int] = {
    def go(count: Int): Process[I, Int] = Await {
      case Some(_) => Emit(count, go(count+1))
      case None => Emit(count, Halt())
    }

    go(0)
  }

  def mean: Process[Double,Double] = {
    def go(mean:Double, count: Int): Process[Double, Double] = Await {
      case Some(num) =>
        val newMean = (mean * count + num) / (count + 1)
        Emit(newMean, go(newMean, count + 1))
      case None => Halt()
    }

    go(0.0, 0)
  }

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
    await((i: I) => f(i,z) match {
      case (o,s2) => emit(o, loop(s2)(f))
    })

  def sumLoop: Process[Double,Double] = loop(0.0)((num, sumSoFar) => (num + sumSoFar, num + sumSoFar))
  def countLoop[I]: Process[I,Int] = loop(0)((_, countSoFar) => (countSoFar + 1, countSoFar + 1))
}


