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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rngNext) = rng.nextInt

    if (i1 == Int.MinValue) (0, rngNext)
    else if (i1 < 0) (Math.abs(i1), rngNext)
    else (i1, rngNext)
  }

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
    ???
  }
}

