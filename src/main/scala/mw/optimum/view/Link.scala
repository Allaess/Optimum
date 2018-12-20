package mw.optimum.view

import scalafx.scene.shape.Line

object Link {
  def apply(start: Vector, weight: Int, end: Vector) = new Line {
    startX = start.x
    startY = start.y
    endX = end.x
    endY = end.y
    strokeWidth = math.min(weight, 60)
  }
}
