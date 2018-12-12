package mw.optimum.view

import scalafx.animation.{ParallelTransition, TranslateTransition}
import mw.optimum.model.{Company, Squad, Tribe}
import scalafx.scene.layout.Pane
import scalafx.util.Duration

class GraphPane extends Pane {
  private var _company = Company()
  private var tribeBubbles = Map.empty[Tribe, Bubble]
  private var squadBubbles = Map.empty[Squad, Bubble]
  private var partOfLinks = Map.empty[(Tribe, Squad), Link]
  private var dependencyLinks = Map.empty[(Tribe, Int, Tribe), Link]
  private var _maxTribeSize = 7
  def maxTribeSize = _maxTribeSize
  def maxTribeSize_=(size: Int) = {
    _maxTribeSize = size
    refresh()
  }
  def company = _company
  def company_=(company: Company) = {
    _company = company
    refresh()
  }
  private def refresh() = {
    val tribes = company.tribes
    val squads = for {
      tribe <- tribes if tribe.size > 1
      squad <- tribe.squads
    } yield squad
    val partOfs = for {
      tribe <- tribes if tribe.size > 1
      squad <- tribe.squads
    } yield (tribe, squad)
    val dependencies = for (triplet <- company.mostCoupled(maxTribeSize)) yield triplet
    tribeBubbles = tribeBubbles.filterKeys(tribes.contains)
    tribeBubbles ++= (for (tribe <- tribes if !tribeBubbles.contains(tribe)) yield tribe -> Bubble(tribe))
    squadBubbles = squadBubbles.filterKeys(squads.contains)
    squadBubbles ++= (for (squad <- squads if !squadBubbles.contains(squad)) yield squad -> Bubble(squad))
    partOfLinks = partOfLinks.filterKeys(partOfs.contains)
    partOfLinks ++= (for (pair@(tribe, squad) <- partOfs if !partOfLinks.contains(pair)) yield
      pair -> Link(tribeBubbles(tribe), 1, squadBubbles(squad)))
    dependencyLinks = dependencyLinks.filterKeys(dependencies.contains)
    dependencyLinks ++= (for (triplet@(tribe1, weight, tribe2) <- dependencies
                              if !dependencyLinks.contains(triplet)) yield
      triplet -> Link(tribeBubbles(tribe1), weight, tribeBubbles(tribe2)))
    val links = partOfLinks.values ++ dependencyLinks.values
    val bubbles = tribeBubbles.values ++ squadBubbles.values
    children = links ++ bubbles
    animate(bubbles)
  }
  def animate(bubbles: Iterable[Bubble]) = {
    val animations=for (bubble1 <- bubbles) yield{
      var (dx, dy) = (0D, 0D)
      for (bubble2 <- bubbles if bubble1 != bubble2) {
        val minDistance = bubble1 minDistance bubble2
        val maxDistance = bubble1 maxDistance bubble2
        val distance = bubble1 distance bubble2
        if (distance < minDistance) {
          val (x, y) = bubble1 towards bubble2
          val correction = (minDistance - distance) / 4
          dx -= x * correction
          dy -= y * correction
        }
        if (distance > maxDistance) {
          val (x, y) = bubble1 towards bubble2
          val correction = (distance - maxDistance) / 4
          dx += x * correction
          dy += y * correction
        }
      }
      new TranslateTransition {
        duration=Duration(100)
        byX = dx
        byY = dy
      }
    }
    val animation=new ParallelTransition(animations.toSeq)
    animation.play()
  }
}
