package mw.optimum.graph

import mw.optimum.model.Squad

trait SquadBubble extends Bubble {
  def squad: Squad
  def parent: TribeBubble
  def correction(that: Bubble) = -(that correction this)
  def correction(that: TribeBubble) = {
    val distance = this distance that
    if (distance < 100) {
      val correction = (100 - distance) / 2
      (this.center - that.center) / distance * correction
    } else {
      val maxDistance = that.layers * 100
      if (distance > maxDistance) {
        val correction = (distance - maxDistance) / 3
        (that.center - this.center) / distance * correction
      } else {
        Vector.zero
      }
    }
  }
  def correction(that: SquadBubble) = {
    val distance = this distance that
    if (distance < 100) {
      val correction = (100 - distance) / 3
      (this.center - that.center) / distance * correction
    } else {
      Vector.zero
    }
  }
  def +(that: Vector) = SquadBubble(squad, parent, center + that)
}
object SquadBubble {
  def apply(_squad: Squad, _parent: TribeBubble, _center: Vector = Vector.random): SquadBubble = new SquadBubble {
    val squad = _squad
    val parent = _parent
    val center = _center
  }
}
