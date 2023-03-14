package exercises03

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = list match {
    case Nil => 0
    case Cons(head: Int, tail: MyList[Int]) => head + sum(tail)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    def helper(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Nil => acc
      case Cons(head: A, tail: MyList[A]) =>
        helper(tail, Cons(head, acc))
    }
    helper(list, Nil)
  }
}
