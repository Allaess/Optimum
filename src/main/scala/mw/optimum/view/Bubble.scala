package mw.optimum.view

import mw.optimum.graph.Vector
import mw.optimum.model.{Squad, Tribe}
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text

trait Bubble extends StackPane {
	def centerX: Double
	def centerY: Double
	def label: String
	def color: Color
	private def format(text: String) = if (text.length > 16) text.take(15) + "..." else text
	val circle = new Circle {
		radius = Bubble.radius
		fill = color
	}
	val text = new Text {
		text = format(label)
		wrappingWidth = Bubble.wrapping
	}
	children = circle :: text :: Nil
	layoutX = centerX - Bubble.radius
	layoutY = centerY - Bubble.radius
}
object Bubble {
	val radius = 40
	val wrapping = 50
	def apply(tribe: Tribe, position: Vector): Bubble =
		if (tribe.size == 1) new Bubble {
			def centerX = position.x
			def centerY = position.y
			def label = tribe.squads.head.name
			def color = LightGreen
		} else new Bubble {
			def centerX = position.x
			def centerY = position.y
			def label = tribe.name
			def color = Yellow
		}
	def apply(squad: Squad, position: Vector): Bubble = new Bubble {
		def centerX = position.x
		def centerY = position.y
		def label = squad.name
		def color = Cyan
	}
}
