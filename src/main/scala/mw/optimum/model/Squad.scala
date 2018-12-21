package mw.optimum.model

trait Squad {
  def name: String
  def weights: Map[Squad, Int]
  def <->(that: Squad) = weights.getOrElse(that, 0)
  override def toString = s"Squad($name)"
  override def equals(obj: Any) = obj match {
    case that: Squad => this.name == that.name
    case _ => false
  }
  override def hashCode = name.hashCode
}
object Squad {
  class Mutable(val name: String) extends Squad {
    var weights = Map.empty[Squad, Int]
    def +=(pair: (Squad, Int)) = weights += pair
  }
}

