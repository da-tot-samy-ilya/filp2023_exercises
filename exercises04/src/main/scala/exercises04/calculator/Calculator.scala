package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def mul(t1: T, t2: T): Result[T]   = Success(t1 * t2)
  def div(t1: T, t2: T): Result[T]   = if (isZero(t2)) DivisionByZero else Success(t1 / t2)
  def plus(t1: T, t2: T): Result[T]  = Success(t1 + t2)
  def minus(t1: T, t2: T): Result[T] = Success(t1 - t2)
  def matching(left: Expr[T], right: Expr[T], func: (T, T) => Result[T], calc: Expr[T] => Result[T]): Result[T] =
    calc(left) match {
      case Success(value1) =>
        calc(right) match {
          case Success(value2) => func(value1, value2)
          case DivisionByZero  => DivisionByZero
        }
      case DivisionByZero => DivisionByZero
    }
  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Mul(left, right)   => matching(left, right, mul, calculate)
    case Div(left, right)   => matching(left, right, div, calculate)
    case Plus(left, right)  => matching(left, right, plus, calculate)
    case Minus(left, right) => matching(left, right, minus, calculate)
    case Val(v)             => Success(v)
    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(value) =>
          if (iff(value)) calculate(left) else calculate(right)
        case DivisionByZero => DivisionByZero
      }
  }
}
