package mw.optimum.view

import mw.optimum.graph.Graph
import mw.optimum.model.{Company, Squad, Tribe}
import scalafx.animation.{FadeTransition, FillTransition, ParallelTransition, TranslateTransition}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.util.Duration
import scala.util.Try

class GraphPane extends Pane {
	private var graph = Graph.empty
	private var maxWeight = 1
	def normalize(weight: Int) = if (maxWeight > 80) weight * 80.0 / maxWeight else weight.toDouble
	def tribeBubbles(graph: Graph) = {
		for (tribe <- graph.company.tribes) yield tribe -> Bubble(tribe, graph.position(tribe))
	}.toMap
	def squadBubbles(graph: Graph) = {
		for {
			tribe <- graph.company.tribes
			squad <- tribe.squads
		} yield squad -> Bubble(squad, graph.position(squad))
	}.toMap
	def prefTribeLink(graph: Graph, maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) =
		graph.company.nextCoupleAndBestScore(maxTribeSize)._1.map { case (tribe1, weight, tribe2) =>
			Link(bubbles(tribe1), bubbles(tribe2), normalize(weight), Red)
		}
	def tribeLinks(graph: Graph, maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) =
		graph.company.mostCoupled(maxTribeSize).map { case (tribe1, weight, tribe2) =>
			Link(bubbles(tribe1), bubbles(tribe2), normalize(weight), Grey)
		}
	def squadLinks(graph: Graph, tribeBubbles: Map[Tribe, Bubble], squadBubbles: Map[Squad, Bubble]) =
		for {
			tribe <- graph.company.tribes if tribe.size > 1
			squad <- tribe.squads
		} yield Link(tribeBubbles(tribe), squadBubbles(squad), normalize(tribe <-> squad))
	def show(graph: Graph, maxTribeSize: Int) = {
		val weights = for {
			tribe <- graph.company.tribes if tribe.size > 1
			squad <- tribe.squads
		} yield tribe <-> squad
		val weight = graph.company.mostCoupled(maxTribeSize).headOption.map(_._2).getOrElse(0)
		maxWeight = Try((weight :: weights).max).getOrElse(0)
		animate(this.graph, graph, maxTribeSize) {
			this.graph = graph
			val tBubbles = tribeBubbles(graph)
			val sBubbles = squadBubbles(graph)
			val pLink = prefTribeLink(graph, maxTribeSize, tBubbles)
			val tLinks = tribeLinks(graph, maxTribeSize, tBubbles).filter(!pLink.contains(_))
			val sLinks = squadLinks(graph, tBubbles, sBubbles)
			children = sLinks ++ tLinks ++ pLink ++ sBubbles.values ++ tBubbles.values
		}
	}
	def show(company: Company, maxTribeSize: Int): Unit = show(Graph(company, this.graph), maxTribeSize)
	def animate(fromGraph: Graph, toGraph: Graph, maxTribeSize: Int)(action: => Any) = {
		val fromTribeBubble = tribeBubbles(fromGraph)
		val toTribeBubble = tribeBubbles(toGraph)
		val fromSquadBubble = squadBubbles(fromGraph)
		val toSquadBubble = squadBubbles(toGraph)
		val step = Duration(2000)
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
		val fadeIns = for (bubble <- addedBubbles) yield new FadeTransition {
			node = bubble
			duration = step
			fromValue = 0
			toValue = 1
		}
		val moves = for {
			(fromBubble, toBubble) <- keptBubbles
			if fromBubble.centerX != toBubble.centerX || fromBubble.centerY != toBubble.centerY
		} yield new TranslateTransition {
			node = fromBubble
			duration = step
			byX = toBubble.centerX - fromBubble.centerX
			byY = toBubble.centerY - fromBubble.centerY
		}
		val recolors = for {
			(fromBubble, toBubble) <- keptBubbles
			if fromBubble.color != toBubble.color
		} yield new FillTransition {
			shape = fromBubble.circle
			duration = step
			fromValue = fromBubble.color
			toValue = toBubble.color
		}
		val fadeOuts = for (bubble <- deletedBubbles) yield new FadeTransition {
			node = bubble
			duration = step
			fromValue = 1
			toValue = 0
		}
		children = addedBubbles ++ keptBubbles.map(_._1) ++ deletedBubbles
		val animation = new ParallelTransition {
			children = fadeIns ++ moves ++ recolors ++ fadeOuts
			onFinished = { _ =>
				action
			}
		}
		animation.play()
	}
}
object GraphPane {
	def apply() = new GraphPane
}
