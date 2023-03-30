package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = (this, other) match {
      case (Right(v1), Right(_)) => Right(v1)
      case (_, Right(v1))        => Right(v1)
      case (_, _)                => this
    }
    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = (this, other) match {
      case (Right(b), Right(bb)) => Right(f(b, bb))
      case (Left(a), _)          => Left(a)
      case (_, Left(aa))         => Left(aa)
    }

    def map[C](f: B => C): Either[A, C] = this match {
      case Right(b) => Right(f(b))
      case Left(a)  => Left(a)
    }

    def flatMap[AA >: A, C](f: B => Either[AA, C]): Either[AA, C] = this match {
      case Right(b) => f(b)
      case Left(a)  => Left(a)
    }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(value) => Right(value)
      case None        => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      list match {
        case Nil            => Right(Nil)
        case ::(head, tail) => f(head).map2(traverse(tail)(f))(_ :: _)
      }
    }

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(identity)
  }
}
