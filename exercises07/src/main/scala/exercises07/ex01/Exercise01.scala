package exercises07.ex01

import exercises07.data.NonEmptyList
import exercises07.typeclasses._

object Exercise01 {
  object Syntax {

    implicit class Fo[F[_], A](private val f_a: F[A]) extends AnyVal {
      def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(f_a)(f)

      def aproduct[B](f_b: F[B])(implicit appl: Applicative[F]): F[(A, B)] =
        appl.product(f_a, f_b)

      def foldLeft[B](b: B)(ff: (B, A) => B)(implicit foldable: Foldable[F]): B = foldable.foldLeft(f_a, b)(ff)

      def app[B](ff: F[A => B])(implicit ap: Applicative[F]): F[B] = Applicative[F].ap(ff)(f_a)

      def combineAll(implicit foldable: Foldable[F], monoid: Monoid[A]): A =
        foldable.foldLeft(f_a, monoid.empty)(monoid.combine)

      def traverse[G[_]: Applicative, B](f: A => G[B])(implicit trav: Traverse[F]): G[F[B]] =
        trav.traverse(f_a)(f)

    }

    implicit class Ao[A](private val a: A) extends AnyVal {
      def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)

      def |+|(b: A)(implicit semigroup: Semigroup[A]): A = semigroup.combine(a, b)
    }
  }

  object Instances {
    import Syntax._

    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      override def empty: String = ""

      override def combine(x: String, y: String): String = x + y

    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y

    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
        fa.foldLeft(List.empty[B].pure[G])((acc, other) =>
          acc
            .aproduct(f(other))
            .map({
              case (acc, next) => acc.appended(next)
            })
        )

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ff.zip(fa).map { case (x, y) => x(y) }

      override def pure[A](a: A): List[A] = List(a)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
          case None    => None
          case Some(x) => Some(f(x))
        }

        def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
          fa match {
            case Some(x) => f(x).map(Some(_))
            case None    => Option.empty[B].pure[G]
          }

        def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
          case None    => b
          case Some(x) => f(b, x)
        }

        def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff match {
          case None => Option.empty[B]
          case Some(f) =>
            fa match {
              case None    => Option.empty[B]
              case Some(x) => Some(f(x))
            }
        }

        def pure[A](a: A): Option[A] = Some(a)
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {
        def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = NonEmptyList(f(fa.head), fa.tail.map(f))

        def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
          f(fa.head).aproduct(fa.tail.traverse(f)).map { case (h, t) => NonEmptyList(h, t) }

        def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B = fa.tail.foldLeft(f(b, fa.head))(f)

        def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] =
          NonEmptyList(ff.head(fa.head), fa.tail.app(ff.tail))

        def pure[A](a: A): NonEmptyList[A] = NonEmptyList(a)
      }

    implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def empty: List[A] = List.empty[A]

      def combine(first: List[A], second: List[A]): List[A] = first ::: second
    }
  }
}
