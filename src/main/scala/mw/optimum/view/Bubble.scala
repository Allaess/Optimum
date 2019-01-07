package mw.optimum.view

import mw.optimum.graph.Vector
import mw.optimum.model.{Squad, Tribe}
import scalafx.scene.control.Tooltip
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
  def tribe: Tribe
  def squadOption: Option[Squad]
  def dragTribe(fromTribeName: String)
  def dragSquad(fromSquadName: String)
  private def format(text: String) = if (text.length > 16) text.take(15) + "..." else text
  def toBuffer = squadOption match {
    case Some(squad) => s"${tribe.name}\t${squad.name}"
    case None => tribe.name
  }
  val circle = new Circle {
    radius = Bubble.radius
    fill = color

  }
  val text = new Text {
    text = format(label)
    wrappingWidth = Bubble.wrapping
  }
  if (label.length > 16) {
    val info = new Tooltip(label)
    Tooltip.install(this, info)
  }
  children = circle :: text :: Nil
  layoutX = centerX - Bubble.radius
  layoutY = centerY - Bubble.radius
  onDragDetected = { event =>
    val buffer = startDragAndDrop(TransferMode.Move)
    val content = new ClipboardContent
    content.putString(this.toBuffer)
    buffer.setContent(content)
    event.consume()
  }
  onDragOver = { event =>
    val buffer = event.getDragboard
    if (buffer.hasString) Bubble.fromBuffer(buffer.getString) match {
      case Some((tribeName, _)) if tribeName != tribe.name =>
        event.acceptTransferModes(TransferMode.Move)
      case _ =>
    }
    event.consume()
  }
  onDragEntered = { event =>
    val buffer = event.getDragboard
    if (buffer.hasString && Bubble.fromBuffer(buffer.getString).nonEmpty) circle.fill = Green
    event.consume()
  }
  onDragExited = { event =>
    val buffer = event.getDragboard
    if (buffer.hasString && Bubble.fromBuffer(buffer.getString).nonEmpty) circle.fill = color
    event.consume()
  }
  onDragDropped = { event =>
    val buffer = event.getDragboard
    if (buffer.hasString) Bubble.fromBuffer(buffer.getString) match {
      case Some((_, Some(squadName))) =>
        dragSquad(squadName)
        event.setDropCompleted(true)
      case Some((tribeName, None)) =>
        dragTribe(tribeName)
        event.setDropCompleted(true)
      case None =>
        event.setDropCompleted(false)
    }
    event.consume()
  }
  onDragDone = _.consume()
}
object Bubble {
  val radius = 40
  val wrapping = 50
  def apply(_tribe: Tribe, position: Vector, maxTribeSize: Int,
            mergeAction: (String, Tribe) => Any, moveAction: (String, Tribe) => Any): Bubble =
    if (_tribe.size == 1) new Bubble {
      def tribe = _tribe
      def squadOption = Some(tribe.squads.head)
      def centerX = position.x
      def centerY = position.y
      def label = _tribe.squads.head.name
      def color = LightGreen
      def dragTribe(fromTribeName: String) = mergeAction(fromTribeName, tribe)
      def dragSquad(fromSquadName: String) = moveAction(fromSquadName, tribe)
    } else if (_tribe.size >= maxTribeSize) new Bubble {
      def tribe = _tribe
      def squadOption = None
      def centerX = position.x
      def centerY = position.y
      def label = _tribe.name
      def color = LightGray
      def dragTribe(fromTribeName: String) = mergeAction(fromTribeName, tribe)
      def dragSquad(fromSquadName: String) = moveAction(fromSquadName, tribe)
    } else new Bubble {
      def tribe = _tribe
      def squadOption = None
      def centerX = position.x
      def centerY = position.y
      def label = _tribe.name
      def color = Yellow
      def dragTribe(fromTribeName: String) = mergeAction(fromTribeName, tribe)
      def dragSquad(fromSquadName: String) = moveAction(fromSquadName, tribe)
    }
  def apply(squad: Squad, position: Vector, _tribe: Tribe, maxTribeSize: Int,
            mergeAction: (String, Tribe) => Any, moveAction: (String, Tribe) => Any): Bubble =
    if (_tribe.size >= maxTribeSize) new Bubble {
      def tribe = _tribe
      def squadOption = Some(squad)
      def centerX = position.x
      def centerY = position.y
      def label = squad.name
      def color = LightGray
      def dragTribe(fromTribeName: String) = mergeAction(fromTribeName, tribe)
      def dragSquad(fromSquadName: String) = moveAction(fromSquadName, tribe)
    } else new Bubble {
      def tribe = _tribe
      def squadOption = Some(squad)
      def centerX = position.x
      def centerY = position.y
      def label = squad.name
      def color = Cyan
      def dragTribe(fromTribeName: String) = mergeAction(fromTribeName, tribe)
      def dragSquad(fromSquadName: String) = moveAction(fromSquadName, tribe)
    }
  def fromBuffer(buffer: String) = buffer.split('\t') match {
    case Array(tribeName) => Some((tribeName, None))
    case Array(tribeName, squadName) => Some((tribeName, Some(squadName)))
    case _ => None
  }
}
