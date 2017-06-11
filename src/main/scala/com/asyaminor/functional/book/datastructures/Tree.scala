package com.asyaminor.functional.book.datastructures


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[T](tree: Tree[T]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) =>
      maximum(left) max maximum(right)
  }

  def depth[T](tree: Tree[T]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) =>
      val leftMax = 1 + depth(left)
      val rightMax = 1 + depth(right)

      leftMax max rightMax
  }

  def map[T,U](tree: Tree[T])(f: T => U): Tree[U] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[T, U](tree: Tree[T])(baseF: T => U)(f:(U, U) => U): U = tree match {
    case Leaf(v) => baseF(v)
    case Branch(left, right) => f(fold(left)(baseF)(f), fold(right)(baseF)(f))
  }

  def sizeWithFold[T](tree: Tree[T]): Int = fold(tree)(_ => 1)((l, r) => l + r)
  def mapWithFold[T, U](tree: Tree[T])(f: T => U): Tree[U] = fold(tree)(t => Leaf(f(t)):Tree[U])((l, r) => Branch(l, r))
}