package mw.optimum.view

import mw.optimum.model.{Squad, Tribe}
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text

abstract class Bubble(position: Vector) extends StackPane {
  def label: String
  def color: Color
  def tribe: Option[Tribe]
  def squad: Option[Squad]
  val circle = new Circle {
    radius = 30
    fill = color
  }
  val text = new Text {
    text = format(label)
    wrappingWidth = 45
  }
  children = circle :: text :: Nil
  layoutX = position.x - 30
  layoutY = position.y - 30
  val centerX = layoutX + 30
  val centerY = layoutY + 30
  def format(label: String) = if (label.length > 16) label.take(15) + "..." else label
}
object Bubble {
  def apply(_tribe: Tribe, position: Vector): Bubble = if (_tribe.size == 1) new Bubble(position) {
    def label = _tribe.squads.head.name
    def color = LightGreen
    def tribe = Some(_tribe)
    def squad = Some(_tribe.squads.head)
  }
  else new Bubble(position) {
    def label = _tribe.name
    def color = Yellow
    def tribe = Some(_tribe)
    def squad = None
  }
  def apply(_squad: Squad, position: Vector): Bubble = new Bubble(position) {
    def label = _squad.name
    def color = Cyan
    def tribe = None
    def squad = Some(_squad)
  }
}
