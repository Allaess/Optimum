package mw.optimum.view

import mw.optimum.graph.Vector
import mw.optimum.model.{Squad, Tribe}
import scalafx.scene.input.{ClipboardContent, TransferMode}
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
	def merge(from: String)
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
	onDragDetected = { event =>
		val buffer = startDragAndDrop(TransferMode.Move)
		val content = new ClipboardContent
		content.putString(label)
		buffer.setContent(content)
		event.consume()
	}
	onDragOver = { event =>
		if (event.getGestureSource != this && event.getDragboard.hasString)
			event.acceptTransferModes(TransferMode.Move)
		event.consume()
	}
	onDragEntered = { event =>
		if (event.getGestureSource != this && event.getDragboard.hasString)
			circle.fill = Green
		event.consume()
	}
	onDragExited = { event =>
		if (event.getGestureSource != this && event.getDragboard.hasString)
			circle.fill = color
		event.consume()
	}
	onDragDropped = { event =>
		val buffer = event.getDragboard
		if (buffer.hasString)
			merge(buffer.getString)
		event.setDropCompleted(buffer.hasString)
		event.consume()
	}
	onDragDone = _.consume()
}
object Bubble {
	val radius = 40
	val wrapping = 50
	def apply(tribe: Tribe, position: Vector, maxTribeSize: Int, action: (String, Tribe) => Any): Bubble =
		if (tribe.size == 1) new Bubble {
			lazy val centerX = position.x
			lazy val centerY = position.y
			lazy val label = tribe.squads.head.name
			lazy val color = LightGreen
			def merge(from: String) = action(from, tribe)
		} else if (tribe.size >= maxTribeSize) new Bubble {
			lazy val centerX = position.x
			lazy val centerY = position.y
			lazy val label = tribe.name
			lazy val color = LightGray
			def merge(from: String) = action(from, tribe)
		} else new Bubble {
			lazy val centerX = position.x
			lazy val centerY = position.y
			lazy val label = tribe.name
			lazy val color = Yellow
			def merge(from: String) = action(from, tribe)
		}
	def apply(squad: Squad, position: Vector, tribe: Tribe, maxTribeSize: Int, action: (String, Tribe) => Any)
	: Bubble =
		if (tribe.size >= maxTribeSize) new Bubble {
			lazy val centerX = position.x
			lazy val centerY = position.y
			lazy val label = squad.name
			lazy val color = LightGray
			def merge(from: String) = action(from, tribe)
		}
		else new Bubble {
			lazy val centerX = position.x
			lazy val centerY = position.y
			lazy val label = squad.name
			lazy val color = Cyan
			def merge(from: String) = action(from, tribe)
		}
}
