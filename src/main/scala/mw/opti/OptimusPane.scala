package mw.opti

import java.io.File
import scalafx.animation.{ParallelTransition, TranslateTransition}
import scalafx.scene.layout.Pane
import scalafx.util.Duration
import scala.collection.mutable

class OptimusPane extends Pane {
	minWidth = 800
	minHeight = 450
	type Entry = (Company, Traversable[Bubble], Traversable[Link])
	var before: List[Entry] = (Company(), Nil, Nil) :: Nil
	var after: List[Entry] = List.empty[Entry]
	def load(file: File) = {
		val company = Company.load(file)
		val start = company.start
		val bubbles = tribeBubbles(company) ++ squadBubbles(company)
		val links = partLinks(company)
		val first = (company, bubbles, links)
		val second = (start, squadBubbles(start), partLinks(start) ++ strongestLinks(start))
		before = first :: Nil
		after = second :: Nil
		children = links ++ bubbles
		animate(links, bubbles)
	}
	def animate(links: Iterable[Link], bubbles: Iterable[Bubble]): Unit = {
		val start = (for (bubble <- bubbles) yield bubble -> bubble.toVector).toMap
		val target = mutable.Map(start.toList: _*)
		for {
			bubble1 <- bubbles
			bubble2 <- bubbles
			distance = bubble1 distance bubble2
			if bubble1 != bubble2 && distance < 100
		} {
			val vector1 = bubble1.toVector
			val vector2 = bubble2.toVector
			val direction = (vector2 - vector1) / distance
			val moveDistance = (100 - distance) / 2
			val correction = direction * moveDistance
			target(bubble1) = vector1 - correction
			target(bubble2) = vector2 + correction
		}
		val counts = mutable.Map((for (bubble <- bubbles) yield bubble -> 0).toList: _*)
		for (link <- links) {
			val bubble1 = link.bubble1
			val bubble2 = link.bubble2
			counts(bubble1) += 1
			counts(bubble2) += 1
		}
		for (link <- links) {
			val bubble1 = link.bubble1
			val bubble2 = link.bubble2
			val distance = bubble1 distance bubble2
			val count1 = counts(bubble1)
			val count2 = counts(bubble2)
			def layer(count: Int) =
				if (count <= 6) 1
				else if (count <= 18) 2
				else 3
			val layer1 = layer(count1)
			val layer2 = layer(count2)
			val layers = if (layer1 > layer2) layer1 else layer2
			val maxDistance = layers * 100
			if (distance > maxDistance) {
				val vector1 = bubble1.toVector
				val vector2 = bubble2.toVector
				val direction = (vector2 - vector1) / distance
				val moveDistance = (distance - maxDistance) / 2
				val correction = direction * moveDistance
				target(bubble1) += correction
				target(bubble1) -= correction
			}
		}
		val animations = for (bubble <- bubbles) yield {
			val vector = target(bubble) - start(bubble)
			new TranslateTransition(Duration(5000), bubble) {
				byX = vector.x
				byY = vector.y
			}
		}
		val animation: ParallelTransition = new ParallelTransition(animations.toSeq)
		animation.play()
		animation.onFinished = { _ =>
			for (bubble <- bubbles) {
				val vector = target(bubble)
				bubble.layoutX = vector.x - 30
				bubble.layoutY = vector.y - 30
			}
			animate(links, bubbles)
		}
	}
	def tribeBubbles(company: Company) =
		for ((_, tribe) <- company.tribes if tribe.size > 1) yield tribe.bubble
	def squadBubbles(company: Company) = for {
		(_, tribe) <- company.tribes
		(_, squad) <- tribe.squads
	} yield squad.bubble
	def partLinks(company: Company) = for {
		(_, tribe) <- company.tribes if tribe.size > 1
		(_, squad) <- tribe.squads
	} yield Link(tribe.bubble, squad.bubble, 1)
	def strongestLinks(company: Company) = {
		val max = company.maxWeight
		for {
			(_, tribe1) <- company.tribes
			(_, tribe2) <- company.tribes
			if (tribe1 <-> tribe2) == max && tribe1.name < tribe2.name
		} yield Link(tribe1.bubble, tribe2.bubble, max)
	}
}
