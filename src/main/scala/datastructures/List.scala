package datastructures
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds:List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(_, t ) => t
    }

  def setHead[A](l: List[A], h:A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def append[A] (al: List[A], a2: List[A]): List[A] =
    al match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]):List[A] = {
    l match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t) (f)
      case _ => as
    }

  def sum1(ints: List[Int]) = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product1(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B):B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double])=
    foldRight(ns, 0.0)((_ * _))

  def length[A](as: List[A]):Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l:List[A], z: B)(f:(B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l:List[A]): List[A] = foldLeft(l, List[A]())((acc, h) =>Cons(h, acc))

  def appendOfFoldLeft[A](l: List[A], r:List[A]):List[A] =
    foldRight(l, r)(Cons(_,_))
}