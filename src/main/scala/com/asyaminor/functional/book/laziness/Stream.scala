package com.asyaminor.functional.book.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionFr: Option[A] = foldRight(None:Option[A])((a, b) => b match {
    case None => Some(a)
    case Some(_) => Some(a)
  })

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }


  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n == 0) Empty
    else Cons(h, () => t().take(n-1))
  }

  def takeU(n: Int): Stream[A] = Stream.unfold[A, (Int, Stream[A])]((n, this)) {
    case (_, Empty) => None
    case (num, Cons(h, t)) => if (num == 0) None else Some(h(), (num - 1, t()))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => if (n == 0) this
    else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(p(h())) Cons(h, () => t().takeWhile(p))
    else t().takeWhile(p)
  }

  def takeWhileU(p: A => Boolean): Stream[A] = Stream.unfold[A, Stream[A]](this) {
    case (Empty) => None
    case (Cons(h, t)) => if (!p(h())) None else Some(h(), t())
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold[(Option[A],Option[B]), (Stream[A], Stream[B])](this, s2) {
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Empty, Empty) => None
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAllFr(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight(Empty:Stream[B])((a, b) => Stream.cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty:Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  def append[B >: A](elem: => Stream[B]): Stream[B] = foldRight(elem)((a,b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (Empty, Empty) => true
    case (_, Empty) => true
    case (Cons(h,t), Cons(hs, ts)) =>
      if (h() == hs()) t().startsWith(ts())
      else false
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def inner(first: Int, second: Int):Stream[Int] = Stream.cons(first, inner(second, first + second))
    inner(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def zipWith[A, B](as: Stream[A], bs: Stream[B]): Stream[(A,B)] = unfold[(A,B), (Stream[A], Stream[B])](as, bs) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
  }

  def fibsU: Stream[Int] = unfold[Int, (Int, Int)]((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromU(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some((s, s + 1)))

  def constantU[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def map[A,B](as: Stream[A])(f: A => B): Stream[B] = unfold((empty[B], as)) {
    case (_, Empty) => None
    case (sb, Cons(ha, ta)) => Some(f(ha()), (sb, ta()))
  }

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}