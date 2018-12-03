sealed trait Option[+A]{
  def map[B](f:A => B):Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  def flatMap[B](f:A => Option[B]):Option[B] =
    map(f) getOrElse None
  def getOrElse[B >:A](default: =>B):B = this match {
    case None => default
    case Some(v) => v
  }
  def orElse[B >: A](ob: => Option[B]):Option[B] =
  def filter(f: A => Boolean):Option[A]
}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]



object sub extends Option {
  def mean(xs:Seq[Double]):Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
