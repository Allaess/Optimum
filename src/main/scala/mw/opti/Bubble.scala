package mw.opti

import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text
import scala.math._
import scala.util.Random

trait Bubble extends StackPane {
	def color: Color
	def txt: String
	val circle = new Circle {
		radius = 30
		fill = color
	}
	val text = new Text {
		text = if (txt.length > 16) txt.take(15) + "..." else txt
		wrappingWidth = 50
	}
	layoutX = Random.nextDouble() * 740
	layoutY = Random.nextDouble() * 390
	children = circle :: text :: Nil
	def distance(that: Bubble) = {
		val dx = this.layoutX() - that.layoutX()
		val dy = this.layoutY() - that.layoutY()
		sqrt(dx * dx + dy * dy)
	}
	def link(that: Bubble)(implicit company: Company)
	def link(that: Tribe)(implicit company: Company)
	def toVector = Vector(layoutX() + circle.radius(), layoutY() + circle.radius())
	def move(vector: Vector) = {
		layoutX = layoutX() + vector.x
		layoutY = layoutY() + vector.y
	}
	override def toString = s"Bubble($txt, ${layoutX()}, ${layoutY()})"
}
object Bubble {
	def apply(squad: Squad) = new Bubble {
		def color = Cyan
		def txt = squad.name
		def link(that: Bubble)(implicit company: Company) = that.link(company.tribe(squad))
		def link(that: Tribe)(implicit company: Company) = ???
	}
	def apply(tribe: Tribe) = new Bubble {
		def color = Yellow
		def txt = tribe.name
		def link(that: Bubble)(implicit company: Company) = that.link(tribe)
		def link(that: Tribe)(implicit company: Company) = ???
	}
}
