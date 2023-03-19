package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  @tailrec
  def sum(list: MyList[Int], acc: Int = 0): Int = list match {
    case Nil                                => acc
    case Cons(head: Int, tail: MyList[Int]) => sum(tail, acc+head)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def helper(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Nil => acc
      case Cons(head: A, tail: MyList[A]) =>
        helper(tail, Cons(head, acc))
    }
    helper(list, Nil)
  }
}
