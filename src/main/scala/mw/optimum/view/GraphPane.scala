package mw.optimum.view

import mw.optimum.graph.Graph
import mw.optimum.model.{Company, Squad, Tribe}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._

class GraphPane extends Pane {
	private var graph = Graph.empty
	def tribeBubbles = {
		for (tribe <- graph.company.tribes) yield tribe -> Bubble(tribe, graph.position(tribe))
	}.toMap
	def squadBubbles = {
		for {
			tribe <- graph.company.tribes
			squad <- tribe.squads
		} yield squad -> Bubble(squad, graph.position(squad))
	}.toMap
	def prefTribeLink(maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) =
		graph.company.nextCoupleAndBestScore(maxTribeSize)._1.map { case (tribe1, weight, tribe2) =>
			Link(bubbles(tribe1), bubbles(tribe2), weight, Red)
		}
	def tribeLinks(maxTribeSize: Int, bubbles: Map[Tribe, Bubble]) =
		graph.company.mostCoupled(maxTribeSize).map { case (tribe1, weight, tribe2) =>
			Link(bubbles(tribe1), bubbles(tribe2), weight, Grey)
		}
	def squadLinks(tribeBubbles: Map[Tribe, Bubble], squadBubbles: Map[Squad, Bubble]) = for {
		tribe <- graph.company.tribes if tribe.size > 1
		squad <- tribe.squads
	} yield Link(tribeBubbles(tribe), squadBubbles(squad))
	def show(graph: Graph, maxTribeSize: Int) = {
		this.graph = graph
		val tBubbles = tribeBubbles
		val sBubbles = squadBubbles
		val pLink = prefTribeLink(maxTribeSize, tBubbles)
		val tLinks = tribeLinks(maxTribeSize, tBubbles)
		val sLinks = squadLinks(tBubbles, sBubbles)
		children = tLinks ++ sLinks ++ pLink ++ sBubbles.values ++ tBubbles.values
	}
	def show(company: Company, maxTribeSize: Int): Unit = show(Graph(company, this.graph), maxTribeSize)
}
object GraphPane {
	def apply() = new GraphPane
}
