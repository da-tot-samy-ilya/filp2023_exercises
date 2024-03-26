package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {
  def apply[A](implicit instance: Eq[A]): Eq[A] = instance
}

object EqInstances {
  implicit val intEq: Eq[Int]         = (a: Int, b: Int) => a == b
  implicit val booleanEq: Eq[Boolean] = (a: Boolean, b: Boolean) => a == b
  implicit def listEq[A](implicit A: Eq[A]): Eq[List[A]] =
    (a: List[A], b: List[A]) => a.corresponds(b)(A.eqv)
  implicit def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
    (a: Option[A], b: Option[A]) =>
      (a, b) match {
        case (Some(x), Some(y)) => A.eqv(x, y)
        case (None, None)       => true
        case _                  => false
      }
}

object EqSyntax {
  implicit class EqOps[A](left: A) {
    def ===(right: A)(implicit A: Eq[A]): Boolean = A.eqv(left, right)
    def !==(right: A)(implicit A: Eq[A]): Boolean = !A.eqv(left, right)
    def eqv(right: A)(implicit A: Eq[A]): Boolean = A.eqv(left, right)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  // 1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
