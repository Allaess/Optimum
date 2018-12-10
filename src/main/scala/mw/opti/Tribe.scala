package mw.opti

class Tribe(val name: String, var squads: Map[String, Squad]) {
	lazy val bubble = Bubble(this)
	def <->(that: Tribe) = {
		val set = for {
			(_, thisSquad) <- this.squads
			(_, thatSquad) <- that.squads
			if thisSquad != thatSquad
		} yield thisSquad <-> thatSquad
		set.sum
	}
	def squad(name: String) = squads.get(name) match {
		case Some(squad) => squad
		case None =>
			val squad = Squad(name)
			squads += name -> squad
			squad
	}
	def size = squads.size
	def +=(squad: Squad) = squads += squad.name -> squad
	override def toString = s"Tribe($name)"
}
object Tribe {
	def apply(name: String) = new Tribe(name, Map.empty)
}
