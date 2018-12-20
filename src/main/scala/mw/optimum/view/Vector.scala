package mw.optimum.view

import scala.util.Random

case class Vector(x: Double, y: Double) {
  def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
  def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
  def unary_- = Vector(-x, -y)
  def *(f: Double) = Vector(x * f, y * f)
  def /(f: Double) = Vector(x / f, y / f)
  def size = math.sqrt(x * x + y * y)
  def vectorTo(that: Vector) = that - this
}
object Vector {
  val zero = Vector(0, 0)
  def random = Vector(Random.nextDouble * 1600, Random.nextDouble * 900)
}
