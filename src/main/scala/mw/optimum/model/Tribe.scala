package mw.optimum.model

trait Tribe {
  def name: String
  def squads: List[Squad]
  def contains(squad: Squad) = squads.contains(squad)
  def ++(that: Tribe) = Tribe(name, this.squads ++ that.squads)
  def squad(name: String) = squads.find(_.name == name)
  def size = squads.size
  def <->(that: Tribe) = {
    val weights = for {
      thisSquad <- this.squads
      thatSquad <- that.squads
    } yield thisSquad <-> thatSquad
    weights.sum
  }
  override def toString = s"Tribe($name)"
}
object Tribe {
  class Mutable(val name: String = newName) extends Tribe {
    var squads = List.empty[Squad.Mutable]
    override def squad(name: String) = squads.find(_.name == name)
    def +=(squad: Squad.Mutable) =
      if (!squads.contains(squad))
        squads ::= squad
  }
  class Immutable(val name: String, val squads: List[Squad]) extends Tribe
  private var count = 0
  private def newName = {
    count += 1
    s"Tribe $count"
  }
  def apply(name: String = newName, squads: List[Squad] = List.empty) = new Immutable(name, squads)
}
