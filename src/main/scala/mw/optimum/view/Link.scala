package mw.optimum.view

import scalafx.scene.shape.Line

case class Link(start: Bubble, weight: Int, end: Bubble) extends Line {
  startX = start.centerX.toDouble
  startY = start.centerY.toDouble
  endX = end.centerX.toDouble
  endY = end.centerY.toDouble
  strokeWidth = math.min(weight, 60)
}
