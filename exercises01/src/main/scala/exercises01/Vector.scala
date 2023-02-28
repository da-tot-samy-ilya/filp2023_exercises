package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x + x, other.y + y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(scalar * x, scalar * y)

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = Math.sqrt(x * x + y * y)

  def normalized(): Vector = {
    val len = euclideanLength
    if (len == 0) {
      return new Vector(0, 0)
    }
    new Vector(x / len, y / len)
  }

  override def equals(other: Any): Boolean = other match {
    case Vector(x, y) => x == this.x && y == this.y
    case _            => false
  }

  // Vector(x, y)
  override def toString: String = s"Vector(${x}, ${y})"
}

object Vector {
  private val nullVector = new Vector(0, 0)
  def fromAngle(angle: Double, length: Double): Vector =
    new Vector(length * Math.sin(Math.PI / 2 - angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector = {
    list.foldLeft(nullVector)(_ + _)
  }

  def unapply(arg: Vector): Option[(Double, Double)] = Some(arg.x, arg.y)
}
