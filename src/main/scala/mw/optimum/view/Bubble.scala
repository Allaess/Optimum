package mw.optimum.view

import mw.optimum.model.{Squad, Tribe}
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text
import scala.util.Random
import scala.math._

abstract class Bubble(val label: String, val color: Color) extends StackPane {
  layoutX = Random.nextDouble * 800
  layoutY = Random.nextDouble * 450
  val centerX = layoutX + 30
  val centerY = layoutY + 30
  def layers: Int
  def minDistance(that: Bubble) = 90D
  def maxDistance(that: Bubble) = (this.layers + that.layers + 1) * 110D
  def distance(that: Bubble) = sqrt(centerX.toDouble * centerX.toDouble + centerY.toDouble * centerY.toDouble)
  def towards(that: Bubble) =
    (that.centerX.toDouble - this.centerX.toDouble, that.centerY.toDouble - this.centerY.toDouble)
  val circle = new Circle {
    radius = 30
    fill = color
  }
  val text = new Text {
    text = label
    wrappingWidth = 40
  }
  children = circle :: text :: Nil
}
object Bubble {
  def apply(squad: Squad): Bubble = new Bubble(format(squad.name), Cyan) {
    val layers = 0
  }
  def apply(tribe: Tribe): Bubble = if (tribe.size == 1) {
    val squad = tribe.squads.head
    new Bubble(format(squad.name), Green) {
      val layers = 0
    }
  } else new Bubble(format(tribe.name), Yellow) {
    def layers = {
      val size = tribe.size
      if (size <= 6) 1
      else if (size <= 18) 2
      else 3
    }
  }
  def format(label: String) = if (label.length > 16) label.take(15) + "..." else label
}
