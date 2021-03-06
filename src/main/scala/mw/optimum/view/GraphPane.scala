package mw.optimum.view

import mw.optimum.graph.{Graph, Vector}
import mw.optimum.model.{Company, Squad, Tribe}
import scalafx.animation._
import scalafx.scene.input.TransferMode
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.util.Duration

import scala.util.Try

trait GraphPane extends Pane {
  private var graph = Graph.empty
  private var maxWeight = 1
  private var maxTribeSize = 0
  def normalize(weight: Int) = if (maxWeight > 80) weight * 80.0 / maxWeight else weight.toDouble
  def tribeBubbles(graph: Graph, maxTribeSize: Int) = {
    for (tribe <- graph.company.tribes) yield tribe -> Bubble(tribe, graph.position(tribe), maxTribeSize, merge, move)
  }.toMap
  def squadBubbles(graph: Graph, maxTribeSize: Int) = {
    for {
      tribe <- graph.company.tribes
      squad <- tribe.squads
    } yield squad -> Bubble(squad, graph.position(squad), tribe, maxTribeSize, merge, move)
  }.toMap
  def merge(fromTribeName: String, toTribe: Tribe): Unit = {
    val fromTribe = graph.company.tribe(fromTribeName).get
    merge(fromTribe, toTribe)
  }
  def move(fromSquadName: String, toTribe: Tribe): Unit = {
    val fromSquad = for {
      tribe <- graph.company.tribes
      squad <- tribe.squads if squad.name == fromSquadName
    } yield squad
    move(fromSquad.head, toTribe)
  }
  def merge(fromTribe: Tribe, toTribe: Tribe)
  def move(fromSquad: Squad, toTribe: Tribe)
  def prefTribeLink(graph: Graph, maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) = {
    val (nextCouple, _) = graph.company.nextCoupleAndBestScore(maxTribeSize)
    nextCouple.map { case (tribe1, weight, tribe2) =>
      (tribe1, tribe2) -> (weight, Link(bubbles(tribe1), bubbles(tribe2), normalize(weight), Red))
    }
  }
  def tribeLinks(graph: Graph, maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) =
    graph.company.mostCoupled(maxTribeSize).map { case (tribe1, weight, tribe2) =>
      (tribe1, tribe2) -> (weight, Link(bubbles(tribe1), bubbles(tribe2), normalize(weight), Cyan))
    }.toMap
  def squadLinks(graph: Graph, tribeBubbles: Map[Tribe, Bubble], squadBubbles: Map[Squad, Bubble]) = {
    for {
      tribe <- graph.company.tribes if tribe.size > 1
      squad <- tribe.squads
      weight = tribe <-> squad
    } yield (squad, tribe) -> (weight, Link(tribeBubbles(tribe), squadBubbles(squad), normalize(weight)))
  }.toMap
  def ignoredLinks(graph: Graph, cutWeight: Int, bubbles: Map[Tribe, Bubble]) = {
    for ((tribe1, weight, tribe2) <- graph.company.ignored(cutWeight)) yield
      (tribe1, tribe2) -> (weight, Link(bubbles(tribe1), bubbles(tribe2), normalize(weight), LightGrey))
  }.toMap
  def show(graph: Graph, maxTribeSize: Int = this.maxTribeSize) = {
    this.maxTribeSize = maxTribeSize
    val tBubbles = tribeBubbles(graph, maxTribeSize)
    val sBubbles = squadBubbles(graph, maxTribeSize)
    val pLink = prefTribeLink(graph, maxTribeSize, tBubbles)
    val pWeight = pLink.map(_._2._1).getOrElse(0)
    val tLinks = tribeLinks(graph, maxTribeSize, tBubbles)
    val sLinks = squadLinks(graph, tBubbles, sBubbles)
    val iLinks = ignoredLinks(graph, pWeight, tBubbles)
    val weights = (iLinks.values ++ sLinks.values ++ tLinks.values ++ pLink.map(_._2)).map(_._1)
    maxWeight = Try(weights.max).getOrElse(0)
    animate(this.graph, graph, maxTribeSize) {
      this.graph = graph
      val tBubbles = tribeBubbles(graph, maxTribeSize)
      val sBubbles = squadBubbles(graph, maxTribeSize)
      val pLink = prefTribeLink(graph, maxTribeSize, tBubbles)
      val pWeight = pLink.map(_._2._1).getOrElse(0)
      val tLinks = tribeLinks(graph, maxTribeSize, tBubbles)
      val sLinks = squadLinks(graph, tBubbles, sBubbles)
      val iLinks = ignoredLinks(graph, pWeight, tBubbles)
      children = (iLinks.values ++ sLinks.values ++ tLinks.values ++ pLink.map(_._2)).map(_._2) ++
        sBubbles.values ++ tBubbles.values
    }
  }
  def show(company: Company, maxTribeSize: Int, pair: Option[(Tribe, Vector)]): Unit = pair match {
    case Some(pair) => show(Graph(company, this.graph + pair), maxTribeSize)
    case None => show(Graph(company, this.graph), maxTribeSize)
  }
  def animate(fromGraph: Graph, toGraph: Graph, maxTribeSize: Int)(action: => Any) = {
    val fromTribeBubble = tribeBubbles(fromGraph, maxTribeSize)
    val toTribeBubble = tribeBubbles(toGraph, maxTribeSize)
    val fromSquadBubble = squadBubbles(fromGraph, maxTribeSize)
    val toSquadBubble = squadBubbles(toGraph, maxTribeSize)
    val step = Duration(500)
    val fromTribes = fromGraph.company.tribes.filter(_.size > 1).toSet
    val toTribes = toGraph.company.tribes.filter(_.size > 1).toSet
    val fromSquads = {
      for {
        tribe <- fromGraph.company.tribes
        squad <- tribe.squads
      } yield squad -> tribe
    }.toMap
    val toSquads = {
      for {
        tribe <- toGraph.company.tribes
        squad <- tribe.squads
      } yield squad -> tribe
    }.toMap
    val addedTribeBubbles = (toTribes -- fromTribes).map { tribe =>
      tribe -> toTribeBubble(tribe)
    }.toMap
    val keptTribeBubbles = (fromTribes intersect toTribes).map { tribe =>
      tribe -> (fromTribeBubble(tribe), toTribeBubble(tribe))
    }.toMap
    val deletedTribeBubbles = (fromTribes -- toTribes).map { tribe =>
      tribe -> fromTribeBubble(tribe)
    }.toMap
    val addedSquadBubbles = (toSquads -- fromSquads.keys).map { case (squad, tribe) =>
      if (tribe.size == 1) squad -> toTribeBubble(tribe)
      else squad -> toSquadBubble(squad)
    }
    val keptSquadBubbles = for {
      (squad, fromTribe) <- fromSquads
      toTribe <- toSquads.get(squad)
    } yield {
      (fromTribe.size, toTribe.size) match {
        case (1, 1) => squad -> (fromTribeBubble(fromTribe), toTribeBubble(toTribe))
        case (1, _) => squad -> (fromTribeBubble(fromTribe), toSquadBubble(squad))
        case (_, 1) => squad -> (fromSquadBubble(squad), toTribeBubble(toTribe))
        case _ => squad -> (fromSquadBubble(squad), toSquadBubble(squad))
      }
    }
    val deletedSquadBubbles = (fromSquads -- toSquads.keys).map { case (squad, tribe) =>
      if (tribe.size == 1) squad -> fromTribeBubble(tribe)
      else squad -> fromSquadBubble(squad)
    }
    val addedBubbles = addedTribeBubbles.values ++ addedSquadBubbles.values
    val keptBubbles = keptTribeBubbles.values ++ keptSquadBubbles.values
    val deletedBubbles = deletedTribeBubbles.values ++ deletedSquadBubbles.values
    val fadeInBubbles = for (bubble <- addedBubbles) yield new FadeTransition {
      node = bubble
      duration = step
      fromValue = 0
      toValue = 1
    }
    val moveBubbles = for {
      (fromBubble, toBubble) <- keptBubbles
      if fromBubble.centerX != toBubble.centerX || fromBubble.centerY != toBubble.centerY
    } yield new TranslateTransition {
      node = fromBubble
      duration = step
      byX = toBubble.centerX - fromBubble.centerX
      byY = toBubble.centerY - fromBubble.centerY
    }
    val recolorBubbles = for {
      (fromBubble, toBubble) <- keptBubbles
      if fromBubble.color != toBubble.color
    } yield new FillTransition {
      shape = fromBubble.circle
      duration = step
      fromValue = fromBubble.color
      toValue = toBubble.color
    }
    val fadeOutBubbles = for (bubble <- deletedBubbles) yield new FadeTransition {
      node = bubble
      duration = step
      fromValue = 1
      toValue = 0
    }
    children = addedBubbles ++ keptBubbles.map(_._1) ++ deletedBubbles
    val animation = new ParallelTransition {
      children = fadeInBubbles ++ moveBubbles ++ recolorBubbles ++ fadeOutBubbles
      onFinished = { _ =>
        action
      }
    }
    animation.play()
  }
  onDragOver = { event =>
    val buffer = event.getDragboard
    if (buffer.hasString && Bubble.fromBuffer(buffer.getString).nonEmpty)
      event.acceptTransferModes(TransferMode.Move)
    event.consume()
  }
  onDragDropped = { event =>
    val buffer = event.getDragboard
    val position = Vector(event.getX, event.getY)
    if (buffer.hasString) Bubble.fromBuffer(buffer.getString) match {
      case Some((_, Some(squadName))) => dragSquad(squadName, position)
      case Some((tribeName, None)) => dragTribe(tribeName, position)
      case None =>
    }
  }
  onDragDone = _.consume()
  def dragSquad(squadName: String, position: Vector) = {
    val list = for {
      tribe <- graph.company.tribes
      squad <- tribe.squads if squad.name == squadName
    } yield (tribe, squad)
    val (tribe, squad) = list.head
    if (tribe.size == 1) show(graph + (tribe -> position))
    else split(squad, position)
  }
  def dragTribe(tribeName: String, position: Vector) = {
    val tribe = graph.company.tribe(tribeName).get
    show(graph + (tribe -> position))
  }
  def split(squad: Squad, position: Vector)
}
object GraphPane {
  def apply(mergeAction: (Tribe, Tribe) => Any,
            moveAction: (Squad, Tribe) => Any,
            splitAction: (Squad, Vector) => Any) = new GraphPane {
    def merge(fromTribe: Tribe, toTribe: Tribe) = mergeAction(fromTribe, toTribe)
    def move(fromSquad: Squad, toTribe: Tribe) = moveAction(fromSquad, toTribe)
    def split(squad: Squad, position: Vector) = splitAction(squad, position)
  }
}
