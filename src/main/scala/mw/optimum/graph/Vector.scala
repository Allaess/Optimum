package mw.optimum.graph

import scala.util.Random

case class Vector(x: Double, y: Double) {
  def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
  def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
  def *(n: Double) = Vector(x * n, y * n)
  def /(n: Double) = Vector(x / n, y / n)
  def size = math.sqrt(x * x + y * y)
  def unary_- = Vector(-x, -y)
}
object Vector {
  val zero = Vector(0, 0)
  def random = Vector(Random.nextDouble * 1600, Random.nextDouble * 900)
}
