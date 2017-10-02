package com.asyaminor.functional.book.algebras

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  // string concatenation is a Monoid
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // list concatenation is also a Monoid
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = true
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = false
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
      case Some(_) => a1
      case _ => a2
    }
    def zero: Option[A] = None
  }

  def endoMonoid[A]= new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    def zero: (A) => A = a => a
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)

  // for exercise purposes
  private def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size > 1) {
      val (left, right) = v.splitAt(v.size / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
    else if (v.isEmpty) m.zero
    else {
      v.map(f).head
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1+c2)
      case (Stub(c1), Part(l, w, r)) => Part(c1 + l, w, r)
      case (Part(l, w, r), Stub(c2)) => Part(l, w, r + c2)
      case (Part(l1, w1, _), Part(_, w2, r2)) => Part(l1, w1 + w2 + 1, r2)
    }
    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def unstub(chars: String) = if (chars.isEmpty) 0 else 1

    val wc = foldMapV(s, wcMonoid)((s) => {Stub(s.toString)})

    wc match {
      case Stub(chars) => unstub(chars)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}
