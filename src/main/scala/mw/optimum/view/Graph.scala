package mw.optimum.view

import mw.optimum.model.{Company, Squad, Tribe}

case class Graph(company: Company, maxTribeSize: Int, positions: Map[Tribe, Vector]) {
  val mostCoupled = company.mostCoupled(maxTribeSize)
  def layout: Graph = {
    val tolerance = 1
    var moved = false
    val corrections = for (thisTribe <- company.tribes) yield {
      val linkedTribes = for ((tribe1, _, tribe2) <- mostCoupled if tribe1 == thisTribe) yield tribe2
      if (linkedTribes.nonEmpty) {
        val corrections = for {
          thatTribe <- linkedTribes
          thisPosition = position(thisTribe)
          thatPosition = position(thatTribe)
          targetDistance = distance(thisTribe, thatTribe)
          corr = correction(thisPosition, thatPosition, targetDistance - 5, targetDistance + 5)
          if corr != Vector.zero
        } yield corr
        val corr =
          if (corrections.isEmpty) Vector.zero
          else (Vector.zero /: corrections) (_ + _) / corrections.size
        moved ||= corr.size > tolerance
        thisTribe -> corr
      } else {
        val corrections = for {
          thatTribe <- company.tribes if thisTribe != thatTribe
          thisPosition = position(thisTribe)
          thatPosition = position(thatTribe)
          minDistance = distance(thisTribe, thatTribe)
          corr = minCorrection(thisPosition, thatPosition, minDistance)
          if corr != Vector.zero
        } yield corr
        val correction =
          if (corrections.isEmpty) Vector.zero
          else (Vector.zero /: corrections) (_ + _) / corrections.size
        moved ||= correction.size > tolerance
        thisTribe -> correction
      }
    }
    if (moved) {
      val newPositions = (for ((tribe, correction) <- corrections) yield
        tribe -> (position(tribe) + correction)).toMap
      new Graph(company, maxTribeSize, positions ++ newPositions).layout
    } else {
      this
    }
  }
  def correction(thisPosition: Vector, thatPosition: Vector, minDistance: Double, maxDistance: Double) = {
    val minCorr = minCorrection(thisPosition, thatPosition, minDistance)
    if (minCorr == Vector.zero) {
      maxCorrection(thisPosition, thatPosition, maxDistance)
    } else minCorr
  }
  def minCorrection(thisPosition: Vector, thatPosition: Vector, minDistance: Double) = {
    val distance = (thisPosition vectorTo thatPosition).size
    if (distance < minDistance) {
      val correction = (minDistance - distance) / 3
      (thatPosition vectorTo thisPosition) * (correction / distance)
    } else Vector.zero
  }
  def maxCorrection(thisPosition: Vector, thatPosition: Vector, maxDistance: Double) = {
    val distance = (thisPosition vectorTo thatPosition).size
    if (distance > maxDistance) {
      val correction = (distance - maxDistance) / 3
      (thisPosition vectorTo thatPosition) * (correction / distance)
    } else Vector.zero
  }
  def translate = {
    val tribeXs = for (tribe <- company.tribes if tribe.size > 0) yield position(tribe).x - 30
    val tribeYs = for (tribe <- company.tribes if tribe.size > 0) yield position(tribe).y - 30
    val squadXs = for {
      tribe <- company.tribes
      squad <- tribe.squads
    } yield position(squad).x - 30
    val squadYs = for {
      tribe <- company.tribes
      squad <- tribe.squads
    } yield position(squad).y - 30
    val minTribeX = if (tribeXs.isEmpty) 0 else tribeXs.min
    val minTribeY = if (tribeYs.isEmpty) 0 else tribeYs.min
    val minSquadX = if (squadXs.isEmpty) 0 else squadXs.min
    val minSquadY = if (squadYs.isEmpty) 0 else squadYs.min
    val correction = Vector(math.min(minTribeX, minSquadX), math.min(minTribeY, minSquadY))
    new Graph(company, maxTribeSize, for ((tribe, position) <- positions) yield tribe -> (position - correction))
  }
  def diameter(tribe: Tribe) = {
    if (tribe.size <= 1) 0
    else if (tribe.size <= 6) 100
    else if (tribe.size <= 18) 200
    else 300
  }
  def distance(tribe1: Tribe, tribe2: Tribe) = diameter(tribe1) + diameter(tribe2) + 100
  def position(tribe: Tribe): Vector = positions(tribe)
  def position(SQUAD: Squad): Vector = (for {
    tribe <- company.tribes
    (SQUAD, index) <- tribe.squads.zipWithIndex
  } yield if (index < 6) {
    val angle = math.toRadians(index * 60)
    position(tribe) + Vector(math.cos(angle) * 100, math.sin(angle) * 100)
  } else if (index < 18) {
    val angle = math.toRadians((index - 6) * 30)
    position(tribe) + Vector(math.cos(angle) * 200, math.sin(angle) * 200)
  } else {
    val angle = math.toRadians((index - 18) * 20)
    position(tribe) + Vector(math.cos(angle) * 300, math.sin(angle) * 300)
  }).head
  def bubbles = {
    val tribes = for (tribe <- company.tribes if tribe.size > 0) yield Bubble(tribe, position(tribe))
    val squads = for {
      tribe <- company.tribes if tribe.size > 1
      squad <- tribe.squads
    } yield Bubble(squad, position(squad))
    squads ++ tribes
  }
  def links = {
    val tribes = for ((tribe1, weight, tribe2) <- mostCoupled) yield Link(position(tribe1), weight, position(tribe2))
    val squads = for {
      tribe <- company.tribes if tribe.size > 1
      squad <- tribe.squads
    } yield Link(position(tribe), 1, position(squad))
    squads ++ tribes
  }
}
object Graph {
  val empty = Graph(Company.empty, 7)
  def apply(company: Company, maxTribeSize: Int): Graph = {
    val positions = for (tribe <- company.tribes) yield tribe -> Vector.random
    Graph(company, maxTribeSize, positions.toMap).layout.translate
  }
  def apply(company: Company, maxTribeSize: Int, previousGraph: Graph): Graph = {
    val positions = for (tribe <- company.tribes) yield {
      if (tribe.size == 1) tribe -> previousGraph.position(tribe.squads.head)
      else tribe -> previousGraph.positions.getOrElse(tribe, Vector.random)
    }
    Graph(company, maxTribeSize, previousGraph.positions ++ positions.toMap).layout.translate
  }
}
