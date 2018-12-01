/**
  * Created by Yomi on 2017/07/03.
  */
object MyModule {
  def abs(n:Int):Int =
    if (n < 0) -n
    else n
  private def formatAbs(x:Int) = {
    val msg = "The abosoulute value of %d is %d"
    msg.format(x,abs(x))
  }
  def factional(n:Int):Int = {
    def go(n:Int,acc:Int):Int =
      if (n <= 0) acc
      else go(n-1,n*acc)
    go(n,1)
  }
  private def formatFactional(n:Int)={
    val msg = "the factional of %d is %d"
    msg.format(n,factional(n))
  }
  def fib(n:Int):Int ={
    def loop(n:Int,second_before:Int,first_before:Int):Int =
      if (n == 0) second_before
      else loop(n-1,first_before,second_before + first_before)
    loop(n,0,1)
  }
  def formatResult(name:String,n:Int,f:Int => Int) ={
    val msg = "The %s of %d is %d"
    msg.format(name,n,f(n))
  }

  def findFirst(as:Array[String],key:String,p:String => Boolean):Int ={
    @annotation.tailrec
    def loop(n:Int):Int =
      if (n >= as.length) -1
      else if(p(as(n))) n
      else loop(n+1)
    loop(0)
  }
  def isSorted[A](as:Array[A],ordered:(A,A) => Boolean):Boolean ={
    def loop(n:Int):Boolean =
    if  (n >= as.length -1) true
    else if (ordered(as(n),as(n+1))) false
    else loop(n + 1)
   loop(0)
  }
  def curry[A,B,C](f:(A,B) => C):A => (B => C) =
    a => b => f(a,b)
  def uncurry[A,B,C](f:A => B => C):(A,B) => C =
    (a,b) => f(a)(b)
//isSorted(Array(1,2,3),(x:Int,y:Int) => x > y)
  def compose[A,B,C](f:B => C,g:A => B):A => C =
    a =>f(g(a))

  def main(args:Array[String]):Unit ={
    println(formatAbs(-42))
    println(fib(9))
    println(formatFactional(factional(7)))
    println(formatResult("factional",7,factional))
  }
}