sealed trait Option[+A]
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]
def failingFn(i:Int):Int ={
  val y = throw new Exception("fail!")
  try {
    val x =  42 + 5
    x + y
  }catch {case e: Exception => 43}
}
def failingFn2(i:Int):Int ={
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail")): Int)
  }catch {case e:Exception => 43}
}

def mean(xs:Sep[Double]):Double =
  if (xs.Empty)
      throw new ArithmeticException("mean of Empty list!")
  else xs.sum / xs.length

def mean_1(xs:Seq[Double],onEmpty:Double):Double =
  if (xs.isEmpty) onEmpty
  else xs.sum / xs.length

def mean_2(xs:Seq[Double]):Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum /xs.length)
