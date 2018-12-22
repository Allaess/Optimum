package mw.optimum.graph

import scala.math._
import scala.util.Random

case class Vector(x: Double, y: Double) {
	def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
	def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
	def unary_- = Vector(-x, -y)
	def *(f: Double) = Vector(x * f, y * f)
	def /(f: Double) = Vector(x / f, y / f)
	def size = sqrt(x * x + y * y)
}
object Vector {
	val zero = Vector(0, 0)
	def random(x: Double, y: Double) = Vector(Random.nextDouble * x, Random.nextDouble * y)
	def average(collection: Iterable[Vector]) =
		if (collection.isEmpty) Vector.zero
		else (Vector.zero /: collection) (_ + _) / collection.size
}
