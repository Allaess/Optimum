package mw.optimum.graph

import mw.optimum.model.Tribe

trait TribeBubble extends Bubble {
  def tribe: Tribe
  def links: Map[Tribe, Int]
  def layers =
    if (tribe.size <= 1) 0
    else if (tribe.size <= 6) 1
    else if (tribe.size <= 18) 2
    else 3
  def correction(that: Bubble) = -(that correction this)
  def correction(that: TribeBubble) = {
    val distance = this distance that
    if (distance < 100) {
      val correction = (100 - distance) / 3
      (this.center - that.center) / distance * correction
    } else if (links.contains(that.tribe)) {
      val maxDistance = (this.layers + that.layers + 1) * 100
      if (distance > maxDistance) {
        val correction = (distance - maxDistance) / 4
        (that.center - this.center) / distance * correction
      } else {
        Vector.zero
      }
    } else {
      Vector.zero
    }
  }
  def correction(that: SquadBubble) = Vector.zero
  def +(that: Vector) = TribeBubble(tribe, links, center + that)
}
object TribeBubble {
  def apply(_tribe: Tribe, _links: Map[Tribe, Int], _center: Vector = Vector.random): TribeBubble = new TribeBubble {
    val tribe = _tribe
    val links = _links
    val center = _center
  }
}
