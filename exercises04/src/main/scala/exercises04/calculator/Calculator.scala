package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Mul(left, right) =>
      calculate(left) match {
        case Success(value1) =>
          calculate(right) match {
            case Success(value2) => Success(value1 * value2)
            case DivisionByZero  => DivisionByZero
          }
        case DivisionByZero => DivisionByZero
      }
    case Div(left, right) =>
      calculate(left) match {
        case Success(value1) =>
          calculate(right) match {
            case Success(value2) =>
              if (isZero(value2)) DivisionByZero else Success(value1 / value2)
            case DivisionByZero => DivisionByZero
          }
        case DivisionByZero => DivisionByZero
      }
    case Plus(left, right) =>
      calculate(left) match {
        case Success(value1) =>
          calculate(right) match {
            case Success(value2) => Success(value1 + value2)
            case DivisionByZero  => DivisionByZero
          }
        case DivisionByZero => DivisionByZero
      }
    case Minus(left, right) =>
      calculate(left) match {
        case Success(value1) =>
          calculate(right) match {
            case Success(value2) => Success(value1 - value2)
            case DivisionByZero  => DivisionByZero
          }
        case DivisionByZero => DivisionByZero
      }
    case Val(v) => Success(v)
    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(value) =>
          if (iff(value)) calculate(left) else calculate(right)
        case DivisionByZero => DivisionByZero
      }
  }
}
