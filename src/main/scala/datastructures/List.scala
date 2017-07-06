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
 def sum3(l:List[Int]):Int = foldLeft(l,0)(_ + _)
 def product3(l:List[Int]):Int = foldLeft(l,1)(_ * _)
 def reverser[A](l:List[A]):List[A] = foldRight(l,List[A])((acc,h) =>Cons(h,acc))
 def appendViaFoldRight[A](l:List[A],r:List[A]):List[A] =
   foldRight(l,r)(Cons(_,_))
 def concat[A](l:List[List[A]]) =
   foldLeft(l,List[A])(append)
 def add(l:List[Int]):List[Int] =
   foldLeft(l,Nil:List[Int])((x,h) => Cons(x + 1,h))
 def doubleToString(l:List[Double]):List[String] =
   foldLeft(l,Nil:List[String])((x,h) => Cons(x.toString,h))
 def map[A,B](as:List[A])(f:A => B):List[B] =
   foldRight(as,Nil:List[A])((h,t) =>Cons(f(h),t))
 def filter[A](as:List[A])(f: A => Boolean):List[A] =
   foldRight(as,Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t )
 def filter_div1[A](l:List[A])(f: A => Boolean):List[A] =
  val buf = new collection.mutable.ListBuffer[A]
  def loop(l:List[A]):Unit = l match {
    case Nil => ()
    case Cons(x,xs) => if (f(x)) buf += x; go(xs)
  }
 loop(l)
 List(buf.toList:_*)
  def flatmap[A,B](as:List[A])(f:A => List[B]):List[B] =
    concat(map(as)(f))
 def fileter3[A](as:List[A])(f:A => Boolean):List[A] =
   flatMap(l)(a => if (f(a)) List(a) else Nil)
  def add_lists(l:List[Int],t:List[Int]):List[Int] = (l,t) match {
    case (_,Nil) => l
    case (Nil,_) => t
    case (Cons(x,xs),Cons(t,ts)) => Cons(x + t,add_lists(xs,ts))
  }
  def zipwith[A](l:List[A],t:List[A])(f: (A,A) => A):List[A] = (l,t) match {
    case (_,Nil) => l
    case (Nil,_) => t
    case (Cons(x,xs),Cons(t,ts)) => Cons(f(x,xs),zipwith(xs,ts)(f))
  }
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}