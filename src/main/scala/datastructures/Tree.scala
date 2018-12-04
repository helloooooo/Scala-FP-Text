package datastructures.Tree


sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A],right:Tree[A]) extends Tree[A]

object Tree{
  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t:Tree[Int]):Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(r))
  }

  def map[A,B](t:Tree[A])(f:A => B):Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f),map(right)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

}