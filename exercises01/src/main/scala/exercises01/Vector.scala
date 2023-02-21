package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x + this.x, other.y + this.y)

  def -(other: Vector): Vector = new Vector(this.x - other.x, this.y - other.y)

  def *(scalar: Double): Vector = new Vector(scalar * this.x, scalar * this.y)

  def unary_- : Vector = new Vector(-this.x, -this.y)

  def euclideanLength: Double = Math.sqrt(this.x * this.x + this.y * this.y)

  def normalized(): Vector = {
    if (this.euclideanLength == 0) {
      return new Vector(0, 0)
    }
    return new Vector(this.x / this.euclideanLength, this.y / this.euclideanLength)
  }

  override def equals(other: Any): Boolean = this.toString == other.toString

  // Vector(x, y)
  override def toString: String = "Vector(" + this.x + ", " + this.y + ")"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector =
    new Vector(length * Math.sin(Math.PI / 2 - angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector = {
    var vector = new Vector(0, 0)
    for (i <- list) {
      vector += i
    }
    return vector
  }

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x, arg.y)
}
