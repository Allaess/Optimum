package mw.opti

import scalafx.scene.shape.Line

class Link(val bubble1: Bubble, val bubble2: Bubble, val strength: Int) extends Line {
	startX <== bubble1.layoutX + 30
	startY <== bubble1.layoutY + 30
	endX <== bubble2.layoutX + 30
	endY <== bubble2.layoutY + 30
	strokeWidth = if (strength < 30) strength else 30
	def length = bubble1 distance bubble2
	def toVector = Vector(endX() - startX(), endY() - startY())
	override def toString = s"Link($bubble1, $bubble2)"
}
object Link {
	def apply(bubble1: Bubble, bubble2: Bubble, strength: Int) = new Link(bubble1, bubble2, strength)
}
