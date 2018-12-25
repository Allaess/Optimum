package mw.optimum.view

import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Line

class Link(bubble1: Bubble, bubble2: Bubble, weight: Double, color: Color) extends Line {
	startX = bubble1.centerX
	startY = bubble1.centerY
	endX = bubble2.centerX
	endY = bubble2.centerY
	strokeWidth = math.max(weight,1)
	stroke = color
}
object Link {
	def apply(bubble1: Bubble, bubble2: Bubble, weight: Double = 1, color: Color = Black): Link =
		new Link(bubble1, bubble2, weight, color)
}
