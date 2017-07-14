package com.asyaminor.functional.book.functional_state

trait RNG {
  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def int: Rand[Int] = rng => rng.nextInt
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rngNext) = rng.nextInt

    if (i1 == Int.MinValue) (0, rngNext)
    else if (i1 < 0) (Math.abs(i1), rngNext)
    else (i1, rngNext)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleM: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def double(rng: RNG): (Double, RNG) = {
    val (nonNeg, next) = nonNegativeInt(rng)

    (nonNeg.toDouble / Int.MaxValue.toDouble, next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, r2) = rng.nextInt
    val (d1, r3) = double(r2)

    ((i1, d1), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d1, r2) = double(rng)
    val (i1, r3) = r2.nextInt

    ((d1, i1), r3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)

    ((d1, d2, d3), r4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (generator, list) = (0 to count).foldLeft((rng, Nil:List[Int]))((tp, b) => {
      val gen = tp._1
      val (i, gNext) = gen.nextInt

      (gNext, i::tp._2)
    })

    (list, generator)
  }
}

