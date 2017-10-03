package com.asyaminor.functional.book.algebras

import com.asyaminor.functional.book.datastructures.{Branch, Leaf, Tree}

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

    val wc = foldMapV(s, wcMonoid)((s) => { Stub(s.toString) })

    wc match {
      case Stub(chars) => unstub(chars)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)
  }

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
  }

  val foldableSeq = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
  }

  val foldableStream = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
  }

  val foldableTree = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
      case Leaf(value) => f(value, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
      case Leaf(value) => f(z, value)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]) = Tree.fold(as)(f)(mb.op)
  }

  val foldableOption = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      } }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[(A) => B] {
      override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
      override def zero: (A) => B = _ => B.zero
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldableSeq.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid[A, Int](intAddition))
  }
}
