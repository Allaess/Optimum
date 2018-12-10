package mw.opti

import scala.math._
import scala.util.Random

case class Vector(x: Double, y: Double) {
	def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
	def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
	def *(f: Double) = Vector(x * f, y * f)
	def /(f: Double) = Vector(x / f, y / f)
	def unary_- = Vector(-x, -y)
	def max(that: Vector) = Vector(if (this.x > that.x) this.x else that.x, if (this.y > that.y) this.y else that.y)
	def size = sqrt(x * x + y * y)
	override def toString = s"Vector($x, $y)"
}
object Vector {
	val origin = Vector(0, 0)
	def random = Vector(Random.nextDouble, Random.nextDouble)
}
