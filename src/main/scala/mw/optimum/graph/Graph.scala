package mw.optimum.graph

import mw.optimum.graph.Graph._
import mw.optimum.model.{Company, Squad, Tribe}
import scala.math._

trait Graph {
  outer =>
  def company: Company
  def positions: Map[Tribe, Vector]
  private lazy val squadPositions = {
    for {
      tribe <- company.tribes
      (squad, indexInTribe) <- tribe.squads.zipWithIndex
    } yield {
      if (tribe.size == 1) squad -> position(tribe)
      else squad -> (position(tribe) + relativePosition(indexInTribe))
    }
  }.toMap
  def position(tribe: Tribe): Vector = positions(tribe)
  def position(squad: Squad): Vector = squadPositions(squad)
  def +(pair: (Tribe, Vector)) = new Graph {
    def company = outer.company
    def positions = outer.positions + pair
  }.layout
  private def relativePosition(index: Int) = {
    def helper(index: Int, layer: Int, size: Int): Vector = {
      if (index < size) {
        val angle = toRadians(360 / size)
        Vector(-cos(angle * index), -sin(angle * index)) * (layer * 100)
      }
      else helper(index - size, layer + 1, size + 6)
    }
    helper(index, 1, 6)
  }
  protected def layout: Graph = {
    var moved = false
    val newPositions = {
      for {
        tribe1 <- company.tribes
        position1 = position(tribe1)
      } yield {
        val vectors = for {
          tribe2 <- company.tribes if tribe1 != tribe2
          position2 = position(tribe2)
          distance = (position2 - position1).size
          minDistance = (layers(tribe1) + layers(tribe2) + 1) * 100
          if distance < minDistance
        } yield {
          val correction = (minDistance - distance) / 3
          (position1 - position2) * (correction / distance)
        }
        val vector = Vector.average(vectors)
        if (vector.size >= 1) moved = true
        val border = layers(tribe1) * 100 + 50
        val newPosition = position1 + vector match {
          case Vector(x, y) if x < border && y < border => Vector(border, border)
          case Vector(x, y) if x < border => Vector(border, y)
          case Vector(x, y) if y < border => Vector(x, border)
          case vec => vec
        }
        tribe1 -> newPosition
      }
    }.toMap
    if (moved) {
      new Graph {
        lazy val company = outer.company
        lazy val positions = newPositions
      }.layout
    } else this
  }
}
object Graph {
  val empty: Graph = new Graph {
    lazy val company = Company.empty
    lazy val positions = Map.empty
  }
  def apply(_company: Company, previous: Graph = Graph.empty): Graph = new Graph {
    lazy val company = _company
    lazy val positions = {
      val random = for (tribe <- company.tribes) yield {
        val buffer = layers(tribe) * 100 + 50
        tribe -> (Vector.random(1600 - buffer * 2, 900 - buffer * 2) + Vector(buffer, buffer))
      }
      val singletonTribes = for (tribe <- _company.tribes if tribe.size == 1) yield {
        tribe -> previous.position(tribe.squads.head)
      }
      random.toMap ++ previous.positions ++ singletonTribes
    }
  }.layout
  private def layers(tribe: Tribe) = {
    def helper(tribeSize: Int, layer: Int, layerSize: Int): Int = {
      if (tribeSize <= layerSize) layer
      else helper(tribeSize - layerSize, layer + 1, layerSize + 6)
    }
    if (tribe.size <= 1) 0
    else helper(tribe.size, 1, 6)
  }
}
