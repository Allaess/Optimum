package mw.optimum.view

import scalafx.scene.shape.Line

class Link(val bubble1: Bubble, val weight: Int, val bubble2: Bubble) extends Line {
  strokeWidth = weight
  startX <== bubble1.centerX
  startY <== bubble1.centerY
  endX <== bubble2.centerX
  endY <== bubble2.centerY
}
object Link {
  def apply(bubble1: Bubble, weight: Int, bubble2: Bubble) = new Link(bubble1, weight, bubble2)
}
