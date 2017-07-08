sealed trait Either[+E,+A]
case class Left[+E](value)