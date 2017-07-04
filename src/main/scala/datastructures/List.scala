/**
  * Created by Yomi on 2017/07/04.
  */
package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object  List{
  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds:List[Double]):Double = ds match{
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as:A*):List[A] =
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail:_*))
  def tail[A](list:List[A]):List[A] = list match{
    case Nil => Nil
    case Cons[_,t]  => xs
  }
  def setHead[A](n:A,list:List[A]):List[A] = list match {
    case Nil => Error
    case Cons(_,t) => Cons(n,t)
  }
  def drop[A](n:Int,list:List[A]):List[A] =
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
  }
  def dropWhile[A](l:List[A], f:A => Boolean):List[A] ={
    l match {
      case Cons(x,xs) if (f(x) == true) => dropWhile(xs,f)
      case _ => l
    }
  }
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def foldRight[A,B](as:List[A],z:B)(f:(A,B) => B):B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }
  def length[A](l:List[A]):Int = foldRight(l,0)((_,acc) => acc + 1)
  def foldLeft[A,B](as:List[A],z:B)(f:(B:A) => B):B = as match {
    case Nil => z
    case Cons(x,xs) => f(foldLeft(xs,z)(f),x)
  }
}