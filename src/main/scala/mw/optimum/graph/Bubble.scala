package mw.optimum.graph

trait Bubble {
  def center: Vector
  def distance(that: Bubble) = (that.center - this.center).size
  def correction(that: Bubble): Vector
  def correction(that: TribeBubble): Vector
  def correction(that: SquadBubble): Vector
  def +(that: Vector): Bubble
}
