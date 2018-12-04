

sealed trait Option[+A]{
  def map[B](f:A => B):Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B) => C):Option[C] =
    a flatMap (aa =>  b map (bb => f(aa,bb)))

  def flatMap[B](f:A => Option[B]):Option[B] =
    map(f) getOrElse None
  def getOrElse[B >:A](default: =>B):B = this match {
    case None => default
    case Some(v) => v
  }
  def orElse[B >: A](ob: => Option[B]):Option[B] =
    this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean):Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]



object Option {
  def mean(xs:Seq[Double]):Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m ,2))))

  def lift[A,B](f:A => B):Option[A] => Option[B] = _ map f

  def parseInsuranceRateQuote(age:String,numberOfSpeedingTickets:String):Option[Double] = {
    val optAge:Option[Int] = Try(age.toInt)
    val optTickets:Option[Int] = Try(numberOfSpeedingTickets.toInt)

  }

  def sequence[A](a:List[Option[A]]):Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh ::_ ))
  }



  def Try[A](a: => A):Option[A] =
    try Some(a)
    catch {case e:Exception => None}
  val absO:Option[Double] => Option[Double] = lift(math.abs)
}
