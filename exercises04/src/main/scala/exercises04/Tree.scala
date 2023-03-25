package exercises04

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @tailrec
    def helper(list: List[Tree[A]], acc: List[B]): List[B] = list match {
      case Nil                         => acc
      case Leaf(value) :: tail         => helper(tail, f(value) :: acc)
      case Branch(left, right) :: tail => helper(left :: right :: tail, acc)
    }
    helper(List(t), Nil).reverse match {
      case head :: tail => tail.foldLeft(head)(g)
    }
  }
  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def max(t: Tree[Int]): Int = fold(t)(identity)(Math.max)

  def depth[A](t: Tree[A]): Int                 = fold(t)(_ => 1)(1 + Math.max(_, _))
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
