package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x + this.x, other.y + this.y)

  def -(other: Vector): Vector = new Vector(this.x - other.x, this.y - other.y)

  def *(scalar: Double): Vector = new Vector(scalar * this.x, scalar * this.y)

  def unary_- : Vector = new Vector(-this.x, -this.y)

  def euclideanLength: Double = Math.sqrt(this.x * this.x + this.y * this.y)

  def normalized(): Vector = {
    val len = this.euclideanLength
    if (len == 0) {
      return new Vector(0, 0)
    }
    return new Vector(this.x / len, this.y / len)
  }

  override def equals(other: Any): Boolean = other match {
    case Vector(this.x,this.y) => true
    case _ => false
  }

  // Vector(x, y)
  override def toString: String = s"Vector(${this.x}, ${this.y})"
}

object Vector {

  private val nullVector = new Vector(0, 0)
  def fromAngle(angle: Double, length: Double): Vector =
    new Vector(length * Math.sin(Math.PI / 2 - angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector = {
    return list.foldLeft(nullVector)((m, n) => m + n)
  }

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x, arg.y)
}
