//package exercises06.e4_eq
//
//trait Eq[A] {
//  def eqv(a: A, b: A): Boolean
//}
//
//object Eq {
//  def apply[A](implicit eq: Eq[A]): Eq[A] = eq
//}
//
//object EqInstances {
//  implicit val intEq: Eq[Int] = new Eq[Int] {
//    def eqv(a: Int, b: Int): Boolean = a == b
//  }
//
//  implicit def listEq[A](implicit eqA: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
//    def eqv(a: List[A], b: List[A]): Boolean = a.zip(b).forall { case (x, y) => eqA.eqv(x, y) }
//  }
//}
//
//object EqSyntax {
//  implicit class EqOps[A](val a: A) extends AnyVal {
//    def ===(b: A)(implicit eqA: Eq[A]): Boolean = eqA.eqv(a, b)
//
//    def !==(b: A)(implicit eqA: Eq[A]): Boolean = !eqA.eqv(a, b)
//  }
//}
//
//object Examples {
//  import EqInstances._
//  import EqSyntax._
//
//  1 eqv 1 // возвращает true
//  1 === 2 // возвращает false
//  1 !== 2 // возвращает true
//  // 1 === "some-string" // не компилируется
//  // 1 !== Some(2) // не компилируется
//  List(true) === List(true) // возвращает true
//}
